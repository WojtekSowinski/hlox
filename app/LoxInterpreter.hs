{-# LANGUAGE DuplicateRecordFields #-}

module LoxInterpreter
  ( LoxAction,
    ProgramState,
    exec,
    initialize,
    runLoxAction,
  )
where

import Control.Monad.State
import Functions (clock)
import LoxAST
import LoxInternals
import Scope
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Prelude hiding (EQ, GT, LT)
import Data.IORef (newIORef, writeIORef, readIORef)
import StaticAnalysis (errorsIn)

initialize :: IO ProgramState
initialize = do
    clockRef <- newIORef $ Function clock
    return $ fromList [("clock", clockRef)]

exec :: Program -> LoxAction ExitCode
exec program = do
  let errors = errorsIn program
  if null errors
    then loxCatch (mapM_ interpret program >> return ExitSuccess)
        (\err -> liftIO (reportError err) >> return (ExitFailure 1))
    else liftIO (mapM_ reportError errors) >> return (ExitFailure 1)

interpret :: Statement -> LoxAction ()
interpret NOP = return ()
interpret (Eval ex) =
  loxCatch
    (void $ eval ex)
    (\(ln, err) -> loxThrow (ln, "Couldn't evaluate expression - " ++ err))
interpret (Print ex) =
  loxCatch
    (eval ex >>= (liftIO . print))
    (\(ln, err) -> loxThrow (ln, "Couldn't evaluate expression - " ++ err))
interpret (VarInitialize _ varName ex) = do
  initVal <- eval ex >>= (liftIO . newIORef)
  modify $ insertVar varName initVal
interpret (Block statements) = do
  modify $ enterNewScope []
  mapM_ interpret statements
  modify discardLocal
interpret (If cond trueBranch falseBranch) = do
  condVal <- eval cond
  interpret $ if isTruthy condVal then trueBranch else falseBranch
interpret (While cond body) = whileM_ (isTruthy <$> eval cond) (interpret body)
interpret (FunctionDef name params body) = do
  fref <- liftIO $ newIORef undefined
  modify $ insertVar name fref
  currentSate <- get
  liftIO $ writeIORef fref $ Function $ LoxFunction {
    fnName=name,
    fnParams=params,
    fnBody=body,
    closure=currentSate}
interpret (Return _ (Just ex)) = do
  retVal <- eval ex
  loxReturn retVal
interpret (Return _ Nothing) = loxReturn Nil
interpret (CouldNotParse _ _) = undefined
interpret (TooManyParams _) = undefined
interpret (DuplicateParams _) = undefined

eval :: Expression -> LoxAction Value
eval (Literal val) = return val
eval (Not ex) = LitBoolean . not . isTruthy <$> eval ex
eval (Negative line ex) = do
  operand <- eval ex
  case operand of
    LitNumber val -> return $ LitNumber (-val)
    _ -> loxThrow (line, "Operand of a negation must be a number.")
eval (And leftEx rightEx) = do
  leftOp <- eval leftEx
  if isTruthy leftOp then eval rightEx else return leftOp
eval (Or leftEx rightEx) = do
  leftOp <- eval leftEx
  if not $ isTruthy leftOp then eval rightEx else return leftOp
eval (BinOperation line leftEx op rightEx) = do
  leftOp <- eval leftEx
  rightOp <- eval rightEx
  applyBinOp line leftOp op rightOp
eval (Variable line varName) = do
  scope <- get
  case lookUp varName scope of
    Nothing -> loxThrow (line, "Undefined Variable " ++ show varName ++ ".")
    Just ref -> liftIO $ readIORef ref
eval (Assign _ target ex) = assign target ex
eval (FunctionCall line funcEx argExs) = do
  func <- eval funcEx
  args <- mapM eval argExs
  case func of
    Function f -> checkArity line f args >> call f args
    _ -> loxThrow (line, "Can't call " ++ show func ++ " as it is not a function.")
eval (TooManyArgs _) = undefined

applyBinOp :: Int -> Value -> BinOp -> Value -> LoxAction Value
applyBinOp _ left EQ right = return $ LitBoolean $ left == right
applyBinOp _ left NEQ right = return $ LitBoolean $ left /= right
applyBinOp _ (LitString s1) ADD (LitString s2) = return $ LitString $ s1 ++ s2
applyBinOp _ (LitNumber n1) op (LitNumber n2) = return $ calc n1 op n2
applyBinOp line _ ADD _ = loxThrow (line, "Operands of '+' must be numbers or strings.")
applyBinOp line _ op _ = loxThrow (line, "Operands of '" ++ show op ++ "' must be numbers.")

calc :: Double -> BinOp -> Double -> Value
calc n1 ADD n2 = LitNumber $ n1 + n2
calc n1 SUB n2 = LitNumber $ n1 - n2
calc n1 MULT n2 = LitNumber $ n1 * n2
calc n1 DIV n2 = LitNumber $ n1 / n2
calc n1 LT n2 = LitBoolean $ n1 < n2
calc n1 GT n2 = LitBoolean $ n1 > n2
calc n1 LEQ n2 = LitBoolean $ n1 <= n2
calc n1 GEQ n2 = LitBoolean $ n1 >= n2
calc _ _ _ = undefined

checkArity :: (LoxCallable f) => Int -> f -> [Value] -> LoxAction ()
checkArity line f args = do
  let argNum = length args
  let expected = arity f
  when (argNum /= expected) $
    loxThrow (line, "Expected " ++ show expected ++ " arguments, got " ++ show argNum ++ ".")

assign :: Expression -> Expression -> LoxAction Value
assign (Variable line varName) ex = do
  scope <- get
  case lookUp varName scope of 
    Nothing -> loxThrow (line, "Undefined variable " ++ show varName ++ ".")
    Just ref -> do
        newVal <- eval ex
        liftIO $ writeIORef ref newVal
        return newVal
assign _ _ = undefined

data LoxFunction = LoxFunction
  { fnName :: Identifier,
    fnParams :: [Identifier],
    fnBody :: Statement,
    closure :: ProgramState
  }

instance Show LoxFunction where
  show f = "<fn " ++ fnName f ++ ">"

instance LoxCallable LoxFunction where
  call :: LoxFunction -> [Value] -> LoxAction Value
  call f args = do
    oldState <- get
    put $ closure f
    argRefs <- mapM (liftIO . newIORef) args
    modify $ enterNewScope $ zip (fnParams f) argRefs
    returnVal <- acceptReturn $ interpret $ fnBody f
    put oldState
    return returnVal
  arity :: LoxFunction -> Int
  arity = length . fnParams
