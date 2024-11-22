module LoxInternals where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, runStateT, modify, gets)
import Control.Monad.Trans.Except (catchE, throwE)
import Data.Char (toLower)
import Scope
import System.Exit (ExitCode (..))

type ProgramState = Scope Value

type LoxAction = ExceptT ShortCircuit (StateT ProgramState IO)

type LoxError = (Int, String)

data ShortCircuit = Errored LoxError | Returned Value

class (Show f) => LoxCallable f where
  call :: f -> [Value] -> LoxAction Value
  arity :: f -> Int

data Value
  = LitString String
  | LitNumber Double
  | LitBoolean Bool
  | Nil
  | forall f. (LoxCallable f) => Function f

instance Show Value where
  show (LitString s) = s
  show (LitNumber n) = if frac == ".0" then whole else number
    where
      (whole, frac) = span (/= '.') number
      number = show n
  show (LitBoolean b) = map toLower $ show b
  show Nil = "nil"
  show (Function f) = show f

isTruthy :: Value -> Bool
isTruthy Nil = False
isTruthy (LitBoolean b) = b
isTruthy _ = True

runLoxAction :: LoxAction a -> ProgramState -> IO (ExitCode, ProgramState)
runLoxAction action state = do
  (result, newState) <- runStateT (runExceptT action) state
  case result of
    Left (Errored err) -> do
      reportError err
      return (ExitFailure 2, state)
    _ -> return (ExitSuccess, newState)

loxThrow :: LoxError -> LoxAction a
loxThrow = throwE . Errored

loxCatch :: LoxAction a -> (LoxError -> LoxAction a) -> LoxAction a
loxCatch action handler = catchE action handler'
  where
    handler' (Errored err) = handler err
    handler' (Returned val) = loxReturn val

loxReturn :: Value -> LoxAction a
loxReturn = throwE . Returned

acceptReturn :: LoxAction a -> LoxAction Value
acceptReturn action = do
  catchE (action >> return Nil) handler where
    handler (Returned val) = return val
    handler (Errored err) = loxThrow err

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ mb ma = do
  b <- mb
  when b $ ma >> whileM_ mb ma

reportError :: LoxError -> IO ()
reportError (ln, err) = do
  let errorBanner = "\027[1;31mERROR on line " ++ show ln ++ ": \027[22m"
  putStrLn $ errorBanner ++ err ++ "\027[0m"
