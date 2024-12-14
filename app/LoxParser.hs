{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LoxParser where

import Control.Applicative (Alternative (many, some), optional, (<|>))
import Control.Monad (guard, mfilter, void)
import Data.Char (isAlpha, isDigit)
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe)
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
  testFor letter
  name <- some $ letter <|> digit
  guard (name `notElem` reservedKeywords)
  return name

reservedKeywords :: [String]
reservedKeywords =
  [ "and",
    "class",
    "else",
    "false",
    "for",
    "fun",
    "if",
    "nil",
    "or",
    "print",
    "return",
    "super",
    "this",
    "true",
    "var",
    "while"
  ]

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
  str <- consumeWhile (/= '"')
  mchar '"' <|> panic "Unterminated string"
  return str

pValue :: Parser Value
pValue =
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
pExpression = whitespace *> assignment <* whitespace
  where
    assignment = do
      target <- equality
      value <- optional (mchar '=' *> assignment)
      case value of
        Nothing -> return target
        Just ex ->
          if isValidLValue target
            then return $ Assign target ex
            else panic "Invalid assignment target."

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
        <|> primaryExp

    primaryExp =
      whitespace
        *> ( Literal <$> pValue
               <|> Variable <$> pIdentifier
               <|> (mchar '(' *> pExpression <* (mchar ')' <|> panic "Mismatched Brackets."))
           )
        <* whitespace

synchronize :: ParseError -> Parser Statement
synchronize err = do
  ln <- getLineNr
  findNext $ void (mchar ';') <|> testFor startOfStatement <|> eof
  return $ CouldNotParse ln ("Parse error - " ++ err)

startOfStatement :: Parser ()
startOfStatement = do
  word <- consumeWhile isAlpha
  guard (word `elem` ["class", "for", "fun", "if", "print", "return", "var", "while"])

pStatement :: Parser Statement
pStatement =
  ( do
      whitespace
      s <- pCommand <|> pDeclaration <|> (consumeChar *> panic "Invalid syntax.")
      whitespace
      return s
  )
    <!> synchronize

pCommand :: Parser Statement
pCommand =
  expressionStatement <|> printStatement <|> block

pDeclaration :: Parser Statement
pDeclaration = varDecl -- <|> funcDef

varDecl :: Parser Statement
varDecl = do
  match "var"
  whitespace
  varName <- pIdentifier
  whitespace
  initValue <- optional (mchar '=' *> pExpression)
  mchar ';' <|> panic "Expected ';' after a variable declaration."
  return $ VarInitialize varName $ fromMaybe (Literal Nil) initValue

funcDef :: Parser Statement
funcDef = undefined

printStatement :: Parser Statement
printStatement = do
  match "print"
  expr <- pExpression
  mchar ';' <|> panic "Expected ';' after a print statement."
  return $ Print expr

expressionStatement :: Parser Statement
expressionStatement = do
  expr <- pExpression
  mchar ';' <|> panic "Expected ';' after an expression."
  return $ Eval expr

block :: Parser Statement
block = do
  mchar '{'
  statements <- untilA (mchar '}') pStatement <|> panic "Missing '}'."
  return $ Block statements

pProgram :: Parser Program
pProgram = untilA eof pStatement
