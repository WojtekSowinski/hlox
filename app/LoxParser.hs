{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LoxParser (program, repl) where

import Control.Applicative (Alternative (many, some), optional, (<|>))
import Control.Monad (guard, mfilter, void)
import Data.Char (isAlpha, isDigit)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe)
import Environment (Identifier)
import LoxAST
import LoxInternals (Value (..))
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
boolean = (keyword "true" $> True) <|> (keyword "false" $> False)

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
    assignment :: Parser Expression = do
      target <- disjunction
      line <- getLineNr
      value <- optional (mchar '=' *> assignment)
      case value of
        Nothing -> return target
        Just ex ->
          if isValidLValue target
            then return $ Assign target ex
            else return $ InvalidAssignmentTarget line

    disjunction :: Parser Expression = do
      firstDisjunct <- conjunction
      disjuncts <- many $ keyword "or" *> conjunction
      return $ foldl' Or firstDisjunct disjuncts

    conjunction :: Parser Expression = do
      firstConjunct <- equality
      conjuncts <- many $ keyword "and" *> equality
      return $ foldl' And firstConjunct conjuncts

    equality :: Parser Expression = do
      firstComparand <- comparisonExp
      let comparand = (,) <$> binaryOperator ["==", "!="] <*> comparisonExp
      comparands <- many comparand
      return $ foldl' (\l (op, r) -> BinOperation 1 l op r) firstComparand comparands

    comparisonExp :: Parser Expression = do
      firstComparand <- sumExp
      let comparand = (,) <$> binaryOperator [">=", ">", "<=", "<"] <*> sumExp
      comparands <- many comparand
      return $ foldl' (\l (op, r) -> BinOperation 1 l op r) firstComparand comparands

    sumExp :: Parser Expression = do
      firstTerm <- productExp
      let term = (,) <$> binaryOperator ["-", "+"] <*> productExp
      terms <- many term
      return $ foldl' (\l (op, r) -> BinOperation 1 l op r) firstTerm terms

    productExp :: Parser Expression = do
      firstFactor <- unaryExp
      let factor = (,) <$> binaryOperator ["/", "*"] <*> unaryExp
      factors <- many factor
      return $ foldl' (\l (op, r) -> BinOperation 1 l op r) firstFactor factors

    unaryExp :: Parser Expression =
      mchar '!' *> (Not <$> unaryExp)
        <|> mchar '-' *> (Negative <$> getLineNr <*> unaryExp)
        <|> call

    call :: Parser Expression = do
      expr <- primaryExp
      modifiers <- many ((propertyAccess <|> functionCall) <* whitespace)
      return $ foldl' (\x f -> f x) expr modifiers

    primaryExp :: Parser Expression = do
      whitespace
      line <- getLineNr
      expr <-
        Literal <$> literal
          <|> (This line <$ match "this")
          <|> Variable line <$> identifier
          <|> Super line <$> superMethod
          <|> (mchar '(' *> expression <* (mchar ')' <|> panic "Mismatched Brackets."))
      whitespace
      return expr

superMethod :: Parser Identifier
superMethod = do
  keyword "super"
  whitespace
  mchar '.' <|> panic "Expected '.' after 'super'."
  whitespace
  identifier <|> panic "Expected a superclass method name."

synchronize :: ParseError -> Parser Statement
synchronize err = do
  ln <- getLineNr
  findNext $ void (mchar ';') <|> testFor startOfStatement <|> eof
  return $ CouldNotParse ln err

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
declaration = varDecl <|> funcDecl <|> classDecl

varDecl :: Parser Statement
varDecl = do
  keyword "var"
  line <- getLineNr
  whitespace
  varName <- identifier
  whitespace
  initValue <- optional (mchar '=' *> expression)
  mchar ';' <|> panic "Expected ';' after a variable declaration."
  return $ VarInitialize line varName $ fromMaybe (Literal Nil) initValue

funcDef :: Parser FunctionDef
funcDef = do
  line <- getLineNr
  funcName <- identifier
  whitespace
  params <- parameters
  body <- whitespace *> block
  let wrapper =
        if
          | length params > 255 -> TooManyParams
          | hasDuplicates params -> DuplicateParams
          | otherwise -> FunctionDef
  return $ wrapper line funcName params body

hasDuplicates :: (Eq a) => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x : xs) = x `elem` xs || hasDuplicates xs

funcDecl :: Parser Statement
funcDecl = keyword "fun" *> whitespace *> (FunctionDecl <$> funcDef)

classDecl :: Parser Statement
classDecl = do
  keyword "class"
  whitespace
  line <- getLineNr
  name <- identifier
  whitespace
  super <- optional $ mchar '<' *> whitespace *> expression <* whitespace
  mchar '{' <|> panic "Expected '{' before class body."
  whitespace
  methods <- funcDef `sepBy` whitespace
  whitespace
  mchar '}' <|> panic "Expected '}' after class body."
  return $ ClassDecl line name super methods

propertyAccess :: Parser (Expression -> Expression)
propertyAccess = do
  prop <- mchar '.' *> (identifier <|> panic "Expected a property name after '.'.")
  line <- getLineNr
  return (\obj -> AccessProperty line obj prop)

arguments :: Parser [Expression]
arguments = do
  mchar '('
  whitespace
  args <- expression `sepBy` (whitespace *> mchar ',' <* whitespace)
  whitespace
  mchar ')' <|> panic "Missing ')' after argument list."
  return args

functionCall :: Parser (Expression -> Expression)
functionCall = do
  line <- getLineNr
  args <- arguments
  if length args > 255
    then return $ const $ TooManyArgs line
    else return (\f -> FunctionCall line f args)

parameters :: Parser [Identifier]
parameters = do
  mchar '('
  whitespace
  params <- identifier `sepBy` (whitespace *> mchar ',' <* whitespace)
  whitespace
  mchar ')' <|> panic "Missing ')' after parameters list."
  return params

returnStatement :: Parser Statement
returnStatement = do
  keyword "return"
  line <- getLineNr
  value <- optional expression
  mchar ';' <|> panic "Expected ';' after a return statement."
  return $ Return line value

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
  let endOfBlock = void (testFor (mchar '}')) <|> eof
  statements <- untilP endOfBlock statement
  mchar '}' <|> panic "Missing '}'."
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
