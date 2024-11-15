{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LoxParser (program, repl) where

import Control.Applicative (Alternative (many, some), optional, (<|>))
import Control.Monad (guard, mfilter, void, when)
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

alphaNumeric :: Parser Char
alphaNumeric = letter <|> digit

keyword :: String -> Parser String
keyword expected = do
  word <- many alphaNumeric
  guard (word == expected)
  return expected

identifier :: Parser Identifier
identifier = do
  testFor letter
  name <- many alphaNumeric
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

boolean :: Parser Bool
boolean = (keyword "true" >> return True) <|> (keyword "false" >> return False)

number :: Parser Double
number = do
  whole <- some digit
  frac <- ((:) <$> mchar '.' <*> some digit) <|> return ""
  return $ read $ whole ++ frac

string :: Parser String
string = do
  mchar '"'
  str <- consumeWhile (/= '"')
  mchar '"' <|> panic "Unterminated string"
  return str

literal :: Parser Value
literal =
  LitBoolean <$> boolean
    <|> LitNumber <$> number
    <|> LitString <$> string
    <|> (keyword "nil" >> return Nil)

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

binaryOperator :: [String] -> Parser BinOp
binaryOperator ops = fromJust . flip lookup operatorDict <$> choice (map match ops)

expression :: Parser Expression
expression = whitespace *> assignment <* whitespace
  where
    assignment = do
      target <- disjunction
      value <- optional (mchar '=' *> assignment)
      case value of
        Nothing -> return target
        Just ex ->
          if isValidLValue target
            then return $ Assign target ex
            else panic "Invalid assignment target."

    disjunction = do
      firstDisjunct <- conjunction
      disjuncts <- many $ keyword "or" *> conjunction
      return $ foldl' Or firstDisjunct disjuncts

    conjunction = do
      firstConjunct <- equality
      conjuncts <- many $ keyword "or" *> equality
      return $ foldl' And firstConjunct conjuncts

    equality = do
      firstComparand <- comparisonExp
      let comparand = (,) <$> binaryOperator ["==", "!="] <*> comparisonExp
      comparands <- many comparand
      return $ foldl' (\l (op, r) -> BinOperation l op r) firstComparand comparands

    comparisonExp = do
      firstComparand <- sumExp
      let comparand = (,) <$> binaryOperator [">=", ">", "<=", "<"] <*> sumExp
      comparands <- many comparand
      return $ foldl' (\l (op, r) -> BinOperation l op r) firstComparand comparands

    sumExp = do
      firstTerm <- productExp
      let term = (,) <$> binaryOperator ["-", "+"] <*> productExp
      terms <- many term
      return $ foldl' (\l (op, r) -> BinOperation l op r) firstTerm terms

    productExp = do
      firstFactor <- unaryExp
      let factor = (,) <$> binaryOperator ["/", "*"] <*> unaryExp
      factors <- many factor
      return $ foldl' (\l (op, r) -> BinOperation l op r) firstFactor factors

    unaryExp =
      mchar '!' *> (Not <$> unaryExp)
        <|> mchar '-' *> (Negative <$> unaryExp)
        <|> functionCall

    functionCall = do
      func <- primaryExp
      call <- optional $ mchar '(' *> (expression `sepBy` mchar ',') <* mchar ')'
      case call of
        Nothing -> return func
        Just args -> do 
            when (length args > 255) $ panic "Can't have more than 255 arguments."
            return $ FunctionCall func args

    primaryExp =
      whitespace
        *> ( Literal <$> literal
               <|> Variable <$> identifier
               <|> (mchar '(' *> expression <* (mchar ')' <|> panic "Mismatched Brackets."))
           )
        <* whitespace

synchronize :: ParseError -> Parser Statement
synchronize err = do
  ln <- getLineNr
  findNext $ void (mchar ';') <|> testFor startOfStatement <|> eof
  return $ CouldNotParse ln ("Parse error - " ++ err)

startOfStatement :: Parser ()
startOfStatement = do
  word <- many alphaNumeric
  guard (word `elem` ["class", "for", "fun", "if", "print", "return", "var", "while"])

statement :: Parser Statement
statement =
  ( do
      whitespace
      s <- command <|> declaration <|> (consumeChar *> panic "Invalid syntax.")
      whitespace
      return s
  )
    <!> synchronize

command :: Parser Statement
command =
  expressionStatement
    <|> forLoop
    <|> ifStatement
    <|> printStatement
    <|> whileLoop
    <|> returnStatement
    <|> block

declaration :: Parser Statement
declaration = varDecl <|> funcDef

varDecl :: Parser Statement
varDecl = do
  keyword "var"
  whitespace
  varName <- identifier
  whitespace
  initValue <- optional (mchar '=' *> expression)
  mchar ';' <|> panic "Expected ';' after a variable declaration."
  return $ VarInitialize varName $ fromMaybe (Literal Nil) initValue

funcDef :: Parser Statement
funcDef = do
  keyword "fun"
  whitespace
  funcName <- identifier
  whitespace
  params <- parameters
  body <- whitespace *> command
  return $ FunctionDef funcName params body

parameters :: Parser [Identifier]
parameters = mchar '(' *> (identifier `sepBy` (whitespace *> mchar ',' <* whitespace))

returnStatement :: Parser Statement
returnStatement = do
    keyword "return"
    value <- optional expression
    mchar ';' <|> panic "Expected ';' after a return statement."
    return $ Return value

printStatement :: Parser Statement
printStatement = do
  keyword "print"
  expr <- expression
  mchar ';' <|> panic "Expected ';' after a print statement."
  return $ Print expr

expressionStatement :: Parser Statement
expressionStatement = do
  expr <- expression
  mchar ';' <|> panic "Expected ';' after an expression."
  return $ Eval expr

block :: Parser Statement
block = do
  mchar '{'
  statements <- untilP (mchar '}') statement <|> panic "Missing '}'."
  return $ Block statements

ifStatement :: Parser Statement
ifStatement = do
  keyword "if"
  whitespace
  mchar '(' <|> panic "Expected '(' after 'if'."
  cond <- expression
  mchar ')' <|> panic "Missing ')' after condition."
  trueBranch <- whitespace *> command <* whitespace
  falseBranch <- optional (keyword "else" *> whitespace *> command)
  return $ If cond trueBranch (fromMaybe NOP falseBranch)

whileLoop :: Parser Statement
whileLoop = do
  keyword "while"
  whitespace
  mchar '(' <|> panic "Expected '(' after 'while'."
  cond <- expression
  mchar ')' <|> panic "Missing ')' after condition."
  whitespace
  While cond <$> command

forLoop :: Parser Statement
forLoop = do
  keyword "for"
  whitespace
  mchar '(' <|> panic "Expected '(' after 'for'."
  whitespace
  initStatement <- (mchar ';' >> return NOP) <|> varDecl <|> expressionStatement
  cond <- expression <|> return (Literal $ LitBoolean True)
  mchar ';' <|> panic "Expected ';' after a loop condition."
  inc <- expression <|> return (Literal Nil)
  mchar ')' <|> panic "Missing ')' after for loop initialization clauses."
  whitespace
  body <- command
  return $ Block [initStatement, While cond $ Block [body, Eval inc]]

program :: Parser Program
program = untilP eof statement

repl :: Parser Program
repl = ((: []) . Print <$> calm expression <* eof) <|> program
