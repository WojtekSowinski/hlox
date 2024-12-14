module LoxAST where

data Literal
  = LitString String
  | LitNumber Double
  | LitBoolean Bool
  | Nil

data BinOp = ADD | SUB | MULT | DIV | LT | GT | LEQ | GEQ | EQ | NEQ

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

newtype Block = Block [Statement]
