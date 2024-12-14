module LoxInterpreter
  ( LoxAction,
    ProgramState,
    exec,
    initState,
    runLoxAction,
  )
where

import Control.Monad.State
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import LoxAST
import Scope
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Prelude hiding (EQ, GT, LT)

type ProgramState = Scope

type LoxAction = StateT ProgramState (ExceptT LoxError IO)

type LoxError = (Int, String)

initState :: ProgramState
initState = emptyScope

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ mb ma = do
  b <- mb
  when b $ ma >> whileM_ mb ma

isTruthy :: Value -> Bool
isTruthy Nil = False
isTruthy (LitBoolean b) = b
isTruthy _ = True

reportError :: LoxError -> IO ()
reportError (ln, err) = do
  let errorBanner = "\027[1;31mERROR on line " ++ show ln ++ ": \027[22m"
  putStrLn $ errorBanner ++ err ++ "\027[0m"

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

runLoxAction :: LoxAction a -> ProgramState -> IO (ExitCode, ProgramState)
runLoxAction action st = do
  result <- runExceptT $ runStateT action st
  case result of
    Left err -> do
      reportError err
      return (ExitFailure 2, st)
    Right (_, st') -> return (ExitSuccess, st')

loxThrow :: LoxError -> LoxAction a
loxThrow = lift . throwE

loxCatch :: LoxAction a -> (LoxError -> LoxAction a) -> LoxAction a
loxCatch action handler = do
  st <- get
  let ioHandler a = runStateT (handler a) st
  (a, s) <- lift $ catchE (runStateT action st) ioHandler
  put s
  return a

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
  initVal <- eval ex
  modify $ insertVar varName initVal
interpret (Block statements) = do
  modify enterNew
  mapM_ interpret statements
  modify discardLocal
interpret (If cond trueBranch falseBranch) = do
  condVal <- eval cond
  interpret $ if isTruthy condVal then trueBranch else falseBranch
interpret (While cond body) = whileM_ (isTruthy <$> eval cond) (interpret body)
interpret _ = undefined

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
    EQ -> return $ LitBoolean $ leftOp == rightOp
    NEQ -> return $ LitBoolean $ leftOp /= rightOp
    ADD -> add leftOp rightOp
    _ -> case (leftOp, rightOp) of
      (LitNumber n1, LitNumber n2) -> return $ calc n1 op n2
      _ -> loxThrow (1, "Operands of '" ++ show op ++ "' must be numbers.")
eval (Variable varName) = do
  scope <- get
  case lookUp varName scope of
    Nothing -> loxThrow (1, "Undefined Variable " ++ show varName ++ ".")
    Just val -> return val
eval (Assign target ex) = assign target ex
eval (FunctionCall funcEx argExs) = do
    func <- eval funcEx
    args <- mapM eval argExs
    call func args

assign :: Expression -> Expression -> LoxAction Value
assign (Variable varName) ex = do
  scope <- get
  unless (isDefined varName scope) $ loxThrow (1, "Undefined variable " ++ show varName ++ ".")
  newVal <- eval ex
  modify $ updateVar varName newVal
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
