{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LoxParser where

import Control.Applicative (Alternative (many, some), (<|>))
import Control.Monad (mfilter)
import Data.Char (isAlpha, isDigit)
import Data.List (foldl')
import Data.Maybe (fromJust)
import LoxAST
import ParserCombinators
import Prelude hiding (EQ, GT, LT)

parseTest :: String -> ParseOutput Expression
parseTest input = snd $ runParser pExpression (input, 1)

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
pBool = (match "true" >> return True) <|> (match "false" >> return False)

pNumber :: Parser Double
pNumber = do
  whole <- some digit
  frac <- ((:) <$> mchar '.' <*> some digit) <|> return ""
  return $ read $ whole ++ frac

pString :: Parser String
pString = do
  mchar '"'
  str <- consumeWhile (/= '\"')
  mchar '\"' <|> panic "Unterminated string"
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

pOperator :: [String] -> Parser BinOp
pOperator ops = fromJust . flip lookup operatorDict <$> choice (map match ops)

pExpression :: Parser Expression
pExpression = whitespace *> equality <* whitespace
  where
    equality = do
      firstComparand <- comparisonExp
      let comparand = (,) <$> pOperator ["==", "!="] <*> comparisonExp
      comparands <- many comparand
      return $ foldl' (\l (op, r) -> BinOperation l op r) firstComparand comparands

    comparisonExp = do
      firstComparand <- sumExp
      let comparand = (,) <$> pOperator [">=", ">", "<=", "<"] <*> sumExp
      comparands <- many comparand
      return $ foldl' (\l (op, r) -> BinOperation l op r) firstComparand comparands

    sumExp = do
      firstTerm <- productExp
      let term = (,) <$> pOperator ["-", "+"] <*> productExp
      terms <- many term
      return $ foldl' (\l (op, r) -> BinOperation l op r) firstTerm terms

    productExp = do
      firstFactor <- unaryExp
      let factor = (,) <$> pOperator ["/", "*"] <*> unaryExp
      factors <- many factor
      return $ foldl' (\l (op, r) -> BinOperation l op r) firstFactor factors

    unaryExp =
        mchar '!' *> (Not <$> unaryExp)
        <|> mchar '-' *> (Negative <$> unaryExp)
        <|>primaryExp

    primaryExp =
      whitespace
        *> ( LitExpr <$> pLiteral
               <|> Variable <$> pIdentifier
               <|> (mchar '(' *> pExpression <* (mchar ')' <|> panic "Mismatched Brackets"))
               <|> panic "Expected expression"
           )
        <* whitespace
