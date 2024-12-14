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
  show (LitString s) = show s
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
  | Scope Block
  | VarInitialize {name :: Identifier, value :: Expression}
  | If {condition :: Expression, trueBranch :: Block, falseBranch :: Block}
  | While {condition :: Expression, body :: Block}
  | For {init :: Statement, condition :: Expression, increment :: Statement, body :: Block}
  | FunctionDef {name :: Identifier, params :: [Identifier], body :: Block}
  | Return Expression
  | CouldNotParse {lineNr :: Int, errMsg :: String}
  deriving (Show)

newtype Block = Block [Statement] deriving (Show)

isValidLValue :: Expression -> Bool
isValidLValue (Variable _) = True
isValidLValue _ = False
