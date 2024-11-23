module StaticAnalysis where

import Control.Monad.State (State, evalState, get, modify, put)
import Data.List (sortOn)
import LoxAST (Expression (..), Program, Statement (..))
import LoxInternals (LoxError)
import Scope (Identifier)

errorsIn :: Program -> [LoxError]
errorsIn program = sortOn fst $ concat [f stmt | f <- passes, stmt <- program]

passes :: [Statement -> [LoxError]]
passes = [recursiveVars, invalidReturns None, doubleDeclarations, errorProductions]

recursiveVars :: Statement -> [LoxError]
recursiveVars (VarInitialize line name expr) =
  [(line, "Declaration of variable " ++ name ++ " refers to itself.") | expr `refersTo` name]
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
refersTo (TooManyArgs _) _ = False

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
    then return [(line, "Variable " ++ name ++ " declared multiple times in the same scope.")]
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
errorProductions (CouldNotParse ln err) = [(ln, "Parse error - " ++ err)]
errorProductions (TooManyParams line) = [(line, "Function can't have more than 255 parameters.")]
errorProductions (DuplicateParams line) =
  [(line, "Function definition assigns the same identifier to multiple parameters.")]
errorProductions (Eval expr) = errProdsInExpr expr
errorProductions (Print expr) = errProdsInExpr expr
errorProductions (Return _ (Just expr)) = errProdsInExpr expr
errorProductions (VarInitialize _ _ expr) = errProdsInExpr expr
errorProductions (Block statements) = concatMap errorProductions statements
errorProductions (If cond ifTrue ifFalse) =
  errProdsInExpr cond ++ errorProductions ifTrue ++ errorProductions ifFalse
errorProductions (While cond body) = errProdsInExpr cond ++ errorProductions body
errorProductions (FunctionDef _ _ body) = errorProductions body
errorProductions _ = []

errProdsInExpr :: Expression -> [LoxError]
errProdsInExpr (TooManyArgs line) = [(line, "Function call can't take more than 255 arguments.")]
errProdsInExpr (BinOperation _ left _ right) = errProdsInExpr left ++ errProdsInExpr right
errProdsInExpr (Negative _ operand) = errProdsInExpr operand
errProdsInExpr (Not operand) = errProdsInExpr operand
errProdsInExpr (And left right) = errProdsInExpr left ++ errProdsInExpr right
errProdsInExpr (Or left right) = errProdsInExpr left ++ errProdsInExpr right
errProdsInExpr (Assign _ left right) = errProdsInExpr left ++ errProdsInExpr right
errProdsInExpr (FunctionCall _ f args) = errProdsInExpr f ++ concatMap errProdsInExpr args
errProdsInExpr _ = []
