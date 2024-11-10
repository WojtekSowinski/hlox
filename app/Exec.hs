module Exec where

import Control.Monad.State
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import LoxAST
import System.Exit (exitFailure)
import qualified Data.Map as Map
import Prelude hiding (EQ, GT, LT)

data ProgramState = ProgramState {
    exitOnError :: Bool,
    environment :: Map.Map Identifier Value
    }

type LoxAction = StateT ProgramState (ExceptT RuntimeError IO)

type RuntimeError = (Int, String)

isTruthy :: Value -> Bool
isTruthy Nil = False
isTruthy (LitBoolean b) = b
isTruthy _ = True

runLoxAction :: LoxAction a -> ProgramState -> IO ProgramState
runLoxAction action st = do
  result <- runExceptT $ runStateT action st
  case result of
    Left (ln, err) -> do
      liftIO $ putStrLn $ "ERROR on line " ++ show ln ++ ": " ++ err
      when (exitOnError st) exitFailure
      return st
    Right (_, st') -> return st'

loxThrow :: RuntimeError -> LoxAction a
loxThrow = lift . throwE

loxCatch :: LoxAction a -> (RuntimeError -> LoxAction a) -> LoxAction a
loxCatch action handler = do
  st <- get
  let ioHandler a = runStateT (handler a) st
  (a, s) <- lift $ catchE (runStateT action st) ioHandler
  put s
  return a

exec :: Statement -> LoxAction ()
exec (Eval ex) =
  loxCatch
    (void $ eval ex)
    (\(ln, err) -> loxThrow (ln, "Couldn't evaluate expression. " ++ err))
exec (Print ex) =
  loxCatch
    (eval ex >>= (liftIO . print))
    (\(ln, err) -> loxThrow (ln, "Couldn't evaluate expression. " ++ err))
exec (CouldNotParse ln err) = loxThrow (ln, "Couldn't parse statement. " ++ err)
exec (VarInitialize varName ex) = do
    initVal <- eval ex
    modify (\s -> s {environment = Map.insert varName initVal $ environment s})
exec _ = undefined

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
    env <- gets environment
    case Map.lookup varName env of
        Nothing -> loxThrow (1, "Undefined Variable " ++ show varName ++ ".")
        Just val -> return val
eval (Assign target ex) = assign target ex

assign :: Expression -> Expression -> LoxAction Value
assign (Variable varName) ex = do
    env <- gets environment
    when (Map.notMember varName env) $ loxThrow (1, "Undefined variable " ++ show varName ++ ".")
    newVal <- eval ex
    modify (\s -> s {environment = Map.insert varName newVal env})
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
