{-# LANGUAGE DuplicateRecordFields #-}

module LoxAST where

import LoxInternals (Value)
import Environment (Identifier)
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
  | This {line :: Int}
  | BinOperation {line :: Int, left :: Expression, operator :: BinOp, right :: Expression}
  | Negative {line :: Int, operand :: Expression}
  | Not {operand :: Expression}
  | And {left :: Expression, right :: Expression}
  | Or {left :: Expression, right :: Expression}
  | Assign {left :: Expression, right :: Expression}
  | FunctionCall {line :: Int, function :: Expression, args :: [Expression]}
  | AccessProperty {line :: Int, object :: Expression, property :: Identifier}
  | TooManyArgs {line :: Int}
  | InvalidAssignmentTarget {line :: Int}
  deriving (Show)

data Statement
  = Eval {expr :: Expression}
  | Print {expr :: Expression}
  | VarInitialize {line :: Int, name :: Identifier, value :: Expression}
  | If {cond :: Expression, trueBranch :: Statement, falseBranch :: Statement}
  | While {cond :: Expression, body :: Statement}
  | Return {line :: Int, returnValue :: Maybe Expression}
  | Block {statements :: [Statement]}
  | FunctionDecl FunctionDef
  | ClassDecl {name :: Identifier, super :: Maybe Expression, methods :: [FunctionDef]}
  | NOP
  | CouldNotParse {line :: Int, errMsg :: String}
  deriving (Show)

data FunctionDef
  = FunctionDef {name :: Identifier, params :: [Identifier], body :: Statement}
  | TooManyParams {line :: Int}
  | DuplicateParams {line :: Int}
  deriving (Show)

isValidLValue :: Expression -> Bool
isValidLValue (Variable {}) = True
isValidLValue (AccessProperty {}) = True
isValidLValue _ = False
