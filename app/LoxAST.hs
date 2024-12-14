module LoxAST where

data Literal
  = LitString String
  | LitNumber Double
  | LitBoolean Bool
  | Nil

data ArithOp = ADD | SUB | MULT | DIV

data CompOp = LT | GT | LEQ | GEQ

data EqOp = EQ | NEQ

data BinOp = Arithmetic ArithOp | Comparison CompOp | Equality EqOp

type Identifier = String

data Expression
  = LitExpr Literal
  | Variable Identifier
  | BinOperation {operator :: ArithOp, leftOperand :: Expression, rightOperand :: Expression}
  | Negative Expression
  | Not Expression
  | Concatenate {leftOperand :: Expression, rightOperand :: Expression}
  | And {leftOperand :: Expression, rightOperand :: Expression}
  | Or {leftOperand :: Expression, rightOperand :: Expression}
  | FunctionCall {function :: Expression, args :: [Expression]}

data Statement = Eval Expression
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
