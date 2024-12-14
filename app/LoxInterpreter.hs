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

initialize :: IO ProgramState
initialize = do
    clockRef <- newIORef $ Function clock
    return $ fromList [("clock", clockRef)]

errorsIn :: Statement -> [LoxError]
errorsIn (CouldNotParse ln err) = [(ln, err)]
errorsIn (Block statements) = concatMap errorsIn statements
errorsIn (If _ ifTrue ifFalse) = errorsIn ifTrue ++ errorsIn ifFalse
errorsIn (While _ body) = errorsIn body
errorsIn (FunctionDef _ _ body) = errorsIn body
errorsIn _ = []

exec :: Program -> LoxAction ExitCode
exec program = do
  let errors = concatMap errorsIn program
  if null errors
    then mapM_ interpret program >> return ExitSuccess
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
interpret (CouldNotParse _ _) = undefined
interpret (VarInitialize varName ex) = do
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
    name=name,
    params=params,
    body=body,
    closure=currentSate}
interpret (Return (Just ex)) = do
  retVal <- eval ex
  loxReturn retVal
interpret (Return Nothing) = loxReturn Nil

eval :: Expression -> LoxAction Value
eval (Literal val) = return val
eval (Not ex) = LitBoolean . not . isTruthy <$> eval ex
eval (Negative ex) = do
  operand <- eval ex
  case operand of
    LitNumber val -> return $ LitNumber (-val)
    _ -> loxThrow (1, "Operand of a negation must be a number.")
eval (And leftEx rightEx) = do
  leftOp <- eval leftEx
  if isTruthy leftOp then eval rightEx else return leftOp
eval (Or leftEx rightEx) = do
  leftOp <- eval leftEx
  if not $ isTruthy leftOp then eval rightEx else return leftOp
eval (BinOperation leftEx op rightEx) = do
  leftOp <- eval leftEx
  rightOp <- eval rightEx
  case op of
    EQ -> LitBoolean <$> leftOp `equals` rightOp
    NEQ -> LitBoolean . not <$> (leftOp `equals` rightOp)
    ADD -> add leftOp rightOp
    _ -> case (leftOp, rightOp) of
      (LitNumber n1, LitNumber n2) -> return $ calc n1 op n2
      _ -> loxThrow (1, "Operands of '" ++ show op ++ "' must be numbers.")
eval (Variable varName) = do
  scope <- get
  case lookUp varName scope of
    Nothing -> loxThrow (1, "Undefined Variable " ++ show varName ++ ".")
    Just ref -> liftIO $ readIORef ref
eval (Assign target ex) = assign target ex
eval (FunctionCall funcEx argExs) = do
  func <- eval funcEx
  args <- mapM eval argExs
  case func of
    Function f -> checkArity f args >> call f args
    _ -> loxThrow (1, "Can't call " ++ show func ++ " as it is not a function.")

checkArity :: (LoxCallable f) => f -> [Value] -> LoxAction ()
checkArity f args = do
  let argNum = length args
  let expected = arity f
  when (argNum /= expected) $
    loxThrow (1, "Expected " ++ show expected ++ " arguments, got " ++ show argNum ++ ".")

assign :: Expression -> Expression -> LoxAction Value
assign (Variable varName) ex = do
  scope <- get
  case lookUp varName scope of 
    Nothing -> loxThrow (1, "Undefined variable " ++ show varName ++ ".")
    Just ref -> do
        newVal <- eval ex
        liftIO $ writeIORef ref newVal
        return newVal
assign _ _ = undefined

add :: Value -> Value -> LoxAction Value
add (LitString s1) (LitString s2) = return $ LitString $ s1 ++ s2
add (LitNumber n1) (LitNumber n2) = return $ calc n1 ADD n2
add _ _ = loxThrow (1, "Operands of '+' must be numbers or strings.")

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

equals :: Value -> Value -> LoxAction Bool
equals (LitBoolean b1) (LitBoolean b2) = return $ b1 == b2
equals (LitNumber n1) (LitNumber n2) = return $ n1 == n2
equals (LitString s1) (LitString s2) = return $ s1 == s2
equals Nil Nil = return True
equals (Function _) (Function _) = loxThrow (1, "Can't compare functions for equality.")
equals _ _ = return False

data LoxFunction = LoxFunction
  { name :: Identifier,
    params :: [Identifier],
    body :: Statement,
    closure :: ProgramState
  }

instance Show LoxFunction where
  show f = "<fn " ++ name f ++ ">"

instance LoxCallable LoxFunction where
  call :: LoxFunction -> [Value] -> LoxAction Value
  call f args = do
    oldState <- get
    put $ closure f
    argRefs <- mapM (liftIO . newIORef) args
    modify $ enterNewScope $ zip (params f) argRefs
    returnVal <- acceptReturn $ interpret $ body f
    put oldState
    return returnVal
  arity :: LoxFunction -> Int
  arity = length . params
