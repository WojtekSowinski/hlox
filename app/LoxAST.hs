{-# LANGUAGE DuplicateRecordFields #-}

module LoxAST where

import LoxInternals (Value)
import Scope (Identifier)
import Prelude hiding (EQ, GT, LT)

data BinOp = ADD | SUB | MULT | DIV | LT | GT | LEQ | GEQ | EQ | NEQ

instance Show BinOp where
  show ADD = "+"
  show SUB = "-"
  show MULT = "*"
  show DIV = "/"
  show LT = "<"
  show GT = ">"
  show LEQ = "<="
  show GEQ = ">="
  show EQ = "=="
  show NEQ = "!="

type Program = [Statement]

data Expression
  = Literal {value :: Value}
  | Variable {line :: Int, name :: Identifier}
  | BinOperation {line :: Int, left :: Expression, operator :: BinOp, right :: Expression}
  | Negative {line :: Int, operand :: Expression}
  | Not {operand :: Expression}
  | And {left :: Expression, right :: Expression}
  | Or {left :: Expression, right :: Expression}
  | Assign {line :: Int, left :: Expression, right :: Expression}
  | FunctionCall {line :: Int, function :: Expression, args :: [Expression]}
  | TooManyArgs {line :: Int}
  deriving (Show)

data Statement
  = Eval {expr :: Expression}
  | Print {expr :: Expression}
  | VarInitialize {line :: Int, name :: Identifier, value :: Expression}
  | If {cond :: Expression, trueBranch :: Statement, falseBranch :: Statement}
  | While {cond :: Expression, body :: Statement}
  | FunctionDef {name :: Identifier, params :: [Identifier], body :: Statement}
  | Return {line :: Int, returnValue :: Maybe Expression}
  | Block {statements :: [Statement]}
  | NOP
  | CouldNotParse {line :: Int, errMsg :: String}
  | TooManyParams {line :: Int}
  | DuplicateParams {line :: Int}
  deriving (Show)

isValidLValue :: Expression -> Bool
isValidLValue (Variable _ _) = True
isValidLValue _ = False
