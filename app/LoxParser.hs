{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LoxParser where

import Control.Applicative (Alternative (many, some), (<|>))
import Control.Monad (mfilter)
import Data.Char (isDigit, isAlpha)
import Data.List (foldl')
import Data.Maybe (fromJust)
import LoxAST
import ParserCombinators
import Prelude hiding (EQ, GT, LT)

whitespace :: Parser String
whitespace = consumeWhile (`elem` " \r\t\n")

digit :: Parser Char
digit = mfilter isDigit consumeChar

letter :: Parser Char
letter = mfilter (\c -> isAlpha c || c == '_') consumeChar

pIdentifier :: Parser Identifier
pIdentifier = do
    firstChar <- letter
    theRest <- many $ letter <|> digit
    return $ firstChar : theRest

pBool :: Parser Bool
pBool =
  (match "true" >> return True)
    <|> (match "false" >> return False)
    <|> fail "Invalid boolean value. Expected \"true\" or \"false\""

pNumber :: Parser Double
pNumber = do
  whole <- some digit
  frac <- ((:) <$> mchar '.' <*> some digit) <|> return ""
  return $ read $ whole ++ frac

pString :: Parser String
pString = do
  mchar '"'
  str <- consumeWhile (/= '\"')
  mchar '\"' <|> fail "Unterminated string"
  return str

pLiteral :: Parser Literal
pLiteral =
  LitBoolean <$> pBool
    <|> LitNumber <$> pNumber
    <|> LitString <$> pString
    <|> (match "nil" >> return Nil)

operatorDict :: [(String, BinOp)]
operatorDict =
  [ ("+", ADD),
    ("-", SUB),
    ("*", MULT),
    ("/", DIV),
    ("<", LT),
    (">", GT),
    ("<=", LEQ),
    (">=", GEQ),
    ("==", EQ),
    ("!=", NEQ)
  ]

pOperators :: [String] -> Parser BinOp
pOperators ops = fromJust . flip lookup operatorDict <$> choice (map match ops)

pExpression :: Parser Expression
pExpression = whitespace *> equality <* whitespace
  where
    equality = do
      firstComparand <- comparison
      let comparand = (,) <$> pOperators ["==", "!="] <*> comparison
      comparands <- many comparand
      return $ foldl' (\l (op, r) -> BinOperation l op r) firstComparand comparands

    comparison = do
      firstComparand <- sum
      let comparand = (,) <$> pOperators [">=", ">", "<=", "<"] <*> sum
      comparands <- many comparand
      return $ foldl' (\l (op, r) -> BinOperation l op r) firstComparand comparands

    sum = do
      firstTerm <- product
      let term = (,) <$> pOperators ["-", "+"] <*> product
      terms <- many term
      return $ foldl' (\l (op, r) -> BinOperation l op r) firstTerm terms

    product = do
      firstFactor <- unary
      let factor = (,) <$> pOperators ["/", "*"] <*> unary
      factors <- many factor
      return $ foldl' (\l (op, r) -> BinOperation l op r) firstFactor factors

    unary =
      primary
        <|> mchar '!' *> (Not <$> unary)
        <|> mchar '-' *> (Negative <$> unary)

    primary =
      LitExpr <$> pLiteral
        <|> Variable <$> pIdentifier
        <|> (mchar '(' *> pExpression <* mchar ')')
