module Exec where

import Control.Monad.State
import Control.Monad.Trans.Except (ExceptT, catchE, throwE)
import LoxAST
import System.Exit (exitFailure)
import Prelude hiding (EQ, GT, LT)

data ProgramState where
  ProgramState :: {exitOnError :: Bool} -> ProgramState

type LoxAction = StateT ProgramState (ExceptT String IO)

isTruthy :: Value -> Bool
isTruthy Nil = False
isTruthy (LitBoolean b) = b
isTruthy _ = True

loxThrow :: String -> LoxAction a
loxThrow = lift . throwE

loxCatch :: LoxAction a -> (String -> LoxAction a) -> LoxAction a
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
    (\err -> exec (HadError 1 $ "Couldn't evaluate expression. " ++ err))
exec (Print ex) =
  loxCatch
    (eval ex >>= (liftIO . print))
    (\err -> exec (HadError 1 $ "Couldn't evaluate expression. " ++ err))
exec (HadError ln err) = do
  liftIO $ putStrLn $ "ERROR on line " ++ show ln ++ ": " ++ err
  shouldExit <- gets exitOnError
  when shouldExit $ liftIO exitFailure
exec _ = undefined

eval :: Expression -> LoxAction Value
eval (LitExpr val) = return val
eval (Not ex) = LitBoolean . not . isTruthy <$> eval ex
eval (Negative ex) = do
  operand <- eval ex
  case operand of
    LitNumber val -> return $ LitNumber (-val)
    _ -> loxThrow "Operand of a negation must be a number."
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
      _ -> loxThrow $ "Operands of '" ++ show op ++ "' must be numbers."

add :: Value -> Value -> LoxAction Value
add (LitString s1) (LitString s2) = return $ LitString $ s1 ++ s2
add (LitNumber n1) (LitNumber n2) = return $ calc n1 ADD n2
add _ _ = loxThrow "Operands of '+' must be numbers or strings."

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
