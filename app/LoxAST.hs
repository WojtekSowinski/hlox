module LoxAST where

import Data.Char (toLower)
import Prelude hiding (EQ, GT, LT)

data Value
  = LitString String
  | LitNumber Double
  | LitBoolean Bool
  | Nil
  deriving (Eq)

instance Show Value where
  show (LitString s) = s
  show (LitNumber n) = show n
  show (LitBoolean b) = map toLower $ show b
  show Nil = "nil"

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

type Identifier = String

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
  deriving (Show)

-- | FunctionCall {function :: Expression, args :: [Expression]}
data Statement
  = Eval Expression
  | Print Expression
  | VarInitialize {name :: Identifier, value :: Expression}
  | If {condition :: Expression, trueBranch :: Statement, falseBranch :: Statement}
  | While {condition :: Expression, body :: Statement}
  | For {init :: Statement, condition :: Expression, increment :: Statement, body :: Statement}
  | FunctionDef {name :: Identifier, params :: [Identifier], body :: Statement}
  | Return Expression
  | CouldNotParse {lineNr :: Int, errMsg :: String}
  | Block [Statement]
  | NOP
  deriving (Show)

isValidLValue :: Expression -> Bool
isValidLValue (Variable _) = True
isValidLValue _ = False
