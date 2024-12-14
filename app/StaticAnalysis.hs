module StaticAnalysis (flagErrors) where

import Control.Monad.State (State, evalState, get, modify, put)
import LoxAST (Expression (..), Program, Statement (..))
import Scope (Identifier)

flagErrors :: Program -> Program
flagErrors = map (invalidReturns None . recursiveVars . doubleDeclarations)

recursiveVars :: Statement -> Statement
recursiveVars stmt@(VarInitialize name expr) =
  if expr `refersTo` name
    then StaticError 1 $ "Declaration of variable " ++ name ++ " refers to itself."
    else stmt
recursiveVars (If cond trueBranch falseBranch) =
  If cond (recursiveVars trueBranch) (recursiveVars falseBranch)
recursiveVars (While cond body) =
  While cond (recursiveVars body)
recursiveVars (FunctionDef name params body) =
  FunctionDef name params (recursiveVars body)
recursiveVars (Block statements) =
  Block (map recursiveVars statements)
recursiveVars stmt = stmt

refersTo :: Expression -> Identifier -> Bool
refersTo (Variable var) name = var == name
refersTo (Literal _) _ = False
refersTo (BinOperation left _ right) name = (left `refersTo` name) || (right `refersTo` name)
refersTo (And left right) name = (left `refersTo` name) || (right `refersTo` name)
refersTo (Or left right) name = (left `refersTo` name) || (right `refersTo` name)
refersTo (Assign left right) name = (left `refersTo` name) || (right `refersTo` name)
refersTo (Negative expr) name = expr `refersTo` name
refersTo (Not expr) name = expr `refersTo` name
refersTo (FunctionCall fn args) name = any (`refersTo` name) (fn : args)

data FunctionType = None | Function

invalidReturns :: FunctionType -> Statement -> Statement
invalidReturns None (Return _) = StaticError 1 "Can't return from outside a function."
invalidReturns _ (FunctionDef name params body) =
  FunctionDef name params (invalidReturns Function body)
invalidReturns t (If cond trueBranch falseBranch) =
  If cond (invalidReturns t trueBranch) (invalidReturns t falseBranch)
invalidReturns t (While cond body) =
  While cond (invalidReturns t body)
invalidReturns t (Block statements) =
  Block (map (invalidReturns t) statements)
invalidReturns _ stmt = stmt

doubleDecls :: Statement -> State [Identifier] Statement
doubleDecls stmt@(VarInitialize name _) = do
  found <- get
  if name `elem` found
    then return $ StaticError 1 $ "Variable " ++ name ++ " declared multiple times in the same scope."
    else modify (name :) >> return stmt
doubleDecls (Block statements) = do
  enclosing <- get
  put []
  scanned <- mapM doubleDecls statements
  put enclosing
  return $ Block scanned
doubleDecls (FunctionDef name params body) =
  FunctionDef name params <$> doubleDecls body
doubleDecls (If cond trueBranch falseBranch) =
  If cond <$> doubleDecls trueBranch <*> doubleDecls falseBranch
doubleDecls (While cond body) = While cond <$> doubleDecls body
doubleDecls stmt = return stmt

doubleDeclarations :: Statement -> Statement
doubleDeclarations stmt = evalState (doubleDecls stmt) []
