module LoxAST where

import Prelude hiding (EQ, GT, LT)
import LoxInternals (Value)
import Scope (Identifier)

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
  = Literal Value
  | Variable Identifier
  | BinOperation {leftOperand :: Expression, operator :: BinOp, rightOperand :: Expression}
  | Negative Expression
  | Not Expression
  | And {leftOperand :: Expression, rightOperand :: Expression}
  | Or {leftOperand :: Expression, rightOperand :: Expression}
  | Assign {target :: Expression, newValue :: Expression}
  | FunctionCall {function :: Expression, args :: [Expression]}
  deriving (Show)

data Statement
  = Eval Expression
  | Print Expression
  | VarInitialize {name :: Identifier, value :: Expression}
  | If {condition :: Expression, trueBranch :: Statement, falseBranch :: Statement}
  | While {condition :: Expression, body :: Statement}
  | FunctionDef {name :: Identifier, params :: [Identifier], body :: Statement}
  | Return (Maybe Expression)
  | CouldNotParse {lineNr :: Int, errMsg :: String}
  | Block [Statement]
  | NOP
  deriving (Show)

isValidLValue :: Expression -> Bool
isValidLValue (Variable _) = True
isValidLValue _ = False
