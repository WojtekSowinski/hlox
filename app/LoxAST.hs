{-# LANGUAGE DuplicateRecordFields #-}

module LoxAST
  ( BinOp (..),
    Program,
    Expression (..),
    Statement (..),
    FunctionDef (..),
    isValidLValue,
  )
where

import Environment (Identifier)
import LoxInternals (Value)
import ParserCombinators (Location)
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
  = Literal Value
  | Variable Location Identifier
  | This Location
  | Super Location Identifier
  | BinOperation Location Expression BinOp Expression
  | Negative Location Expression
  | Not Expression
  | And Expression Expression
  | Or Expression Expression
  | Assign Location Expression Expression
  | FunctionCall Location Expression [Expression]
  | AccessProperty Location Expression Identifier
  deriving (Show)

data Statement
  = Eval Expression
  | Print Expression
  | VarInitialize Location Identifier Expression
  | If Expression Statement Statement
  | While Expression Statement
  | Return Location (Maybe Expression)
  | Block [Statement]
  | FunctionDecl FunctionDef
  | ClassDecl Location Identifier (Maybe Expression) [FunctionDef]
  | NOP
  | CouldNotParse Location String
  deriving (Show)

data FunctionDef
  = FunctionDef {line :: Location, name :: Identifier, params :: [Identifier], body :: Statement}
  deriving (Show)

isValidLValue :: Expression -> Bool
isValidLValue (Variable {}) = True
isValidLValue (AccessProperty {}) = True
isValidLValue _ = False
