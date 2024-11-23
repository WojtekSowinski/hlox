module StaticAnalysis where

import Control.Monad.State (State, evalState, get, modify, put)
import LoxAST (Expression (..), Program, Statement (..))
import Scope (Identifier)
import LoxInternals (LoxError)
import Data.List (sortOn)

errorsIn :: Program -> [LoxError]
errorsIn program = sortOn fst $ concat [f stmt | f <- passes, stmt <- program]

passes :: [Statement -> [LoxError]]
passes = [recursiveVars, invalidReturns None, doubleDeclarations, errorProductions]

recursiveVars :: Statement -> [LoxError]
recursiveVars (VarInitialize line name expr) =
  [( line, "Declaration of variable " ++ name ++ " refers to itself." ) | expr `refersTo` name]
recursiveVars _ = []

refersTo :: Expression -> Identifier -> Bool
refersTo (Variable _ var) name = var == name
refersTo (Literal _) _ = False
refersTo (BinOperation _ left _ right) name = (left `refersTo` name) || (right `refersTo` name)
refersTo (And left right) name = (left `refersTo` name) || (right `refersTo` name)
refersTo (Or left right) name = (left `refersTo` name) || (right `refersTo` name)
refersTo (Assign _ left right) name = (left `refersTo` name) || (right `refersTo` name)
refersTo (Negative _ expr) name = expr `refersTo` name
refersTo (Not expr) name = expr `refersTo` name
refersTo (FunctionCall _ fn args) name = any (`refersTo` name) (fn : args)

data FunctionType = None | Function

invalidReturns :: FunctionType -> Statement -> [LoxError]
invalidReturns None (Return line _) = [(line, "Can't return from outside a function.")]
invalidReturns _ (FunctionDef _ _ body) =
  invalidReturns Function body
invalidReturns t (If _ trueBranch falseBranch) =
  invalidReturns t trueBranch ++ invalidReturns t falseBranch
invalidReturns t (While _ body) =
  invalidReturns t body
invalidReturns t (Block statements) =
  concatMap (invalidReturns t) statements
invalidReturns _ _ = []

doubleDeclarations :: Statement -> [LoxError]
doubleDeclarations stmt = evalState (doubleDecls stmt) []

doubleDecls :: Statement -> State [Identifier] [LoxError]
doubleDecls (VarInitialize line name _) = do
  found <- get
  if name `elem` found
    then return [(line , "Variable " ++ name ++ " declared multiple times in the same scope.")]
    else modify (name :) >> return []
doubleDecls (Block statements) = do
  enclosing <- get
  put []
  errors <- concat <$> mapM doubleDecls statements
  put enclosing
  return errors
doubleDecls (FunctionDef _ _ body) = doubleDecls body
doubleDecls (If _ trueBranch falseBranch) = 
    (++) <$> doubleDecls trueBranch <*> doubleDecls falseBranch
doubleDecls (While _ body) = doubleDecls body
doubleDecls _ = return []

errorProductions :: Statement -> [LoxError]
errorProductions (StaticError ln err) = [(ln, err)]
errorProductions (Block statements) = concatMap errorProductions statements
errorProductions (If _ ifTrue ifFalse) = errorProductions ifTrue ++ errorProductions ifFalse
errorProductions (While _ body) = errorProductions body
errorProductions (FunctionDef _ _ body) = errorProductions body
errorProductions _ = []

