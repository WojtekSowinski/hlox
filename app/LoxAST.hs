module LoxAST where

data Literal
  = LitString String
  | LitNumber Double
  | LitBoolean Bool
  | Nil
  deriving (Show)

data BinOp = ADD | SUB | MULT | DIV | LT | GT | LEQ | GEQ | EQ | NEQ deriving (Show)

type Identifier = String

data Expression
  = LitExpr Literal
  | Variable Identifier
  | BinOperation {leftOperand :: Expression, operator :: BinOp, rightOperand :: Expression}
  | Negative Expression
  | Not Expression
  | And {leftOperand :: Expression, rightOperand :: Expression}
  | Or {leftOperand :: Expression, rightOperand :: Expression}
  | FunctionCall {function :: Expression, args :: [Expression]}
  deriving (Show)

data Statement
  = Eval Expression
  | Print Expression
  | Scope Block
  | VarDeclare Identifier
  | VarInitialize {name :: Identifier, value :: Expression}
  | VarAssign {name :: Identifier, value :: Expression}
  | If {condition :: Expression, trueBranch :: Block, falseBranch :: Block}
  | While {condition :: Expression, body :: Block}
  | For {init :: Statement, condition :: Expression, increment :: Statement, body :: Block}
  | FunctionDef {name :: Identifier, params :: [Identifier], body :: Block}
  | Return Expression
  | HadError {lineNr :: Int, errMsg :: String}
  deriving (Show)

newtype Block = Block [Statement] deriving (Show)
