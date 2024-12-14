module LoxInternals where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State ( StateT, runStateT, gets )
import Control.Monad.Trans.Except (catchE, throwE)
import Data.Char (toLower)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import Environment
import qualified Data.Map as Map
import Control.Monad.Trans (liftIO)

data ProgramState = ProgramState {env :: Environment (IORef Value), lineNumber :: Int}

type LoxAction = ExceptT ShortCircuit (StateT ProgramState IO)

type LoxError = (Int, String)

data ShortCircuit = Errored LoxError | Returned Value

class (Show f) => LoxCallable f where
  call :: f -> [Value] -> LoxAction Value
  arity :: f -> Int

class (Show t) => LoxObject t where
  getProperty :: t -> Identifier -> LoxAction Value
  setProperty :: t -> Identifier -> Value -> LoxAction ()

data LoxClass = LoxClass {name :: Identifier, superclass :: Maybe LoxClass}

instance Show LoxClass where
  show :: LoxClass -> String
  show c = "<class " ++ name c ++ ">"

instance LoxCallable LoxClass where
  call :: LoxClass -> [Value] -> LoxAction Value
  call c _ = do
    props <- liftIO $ newIORef Map.empty
    return $ Object LoxInstance {klass = c, properties = props}
  arity :: LoxClass -> Int
  arity _ = 0

data LoxInstance = LoxInstance {klass :: LoxClass, properties :: IORef (Map.Map Identifier Value)}

instance Show LoxInstance where
  show :: LoxInstance -> String
  show obj = "<instnace of class " ++ name (klass obj) ++ ">"

instance LoxObject LoxInstance where
  getProperty :: LoxInstance -> Identifier -> LoxAction Value
  getProperty obj prop = do
    props <- liftIO $ readIORef $ properties obj
    case Map.lookup prop props of
      Just val -> return val
      Nothing -> loxThrow ("Undefined property '" ++ prop ++ "'.")
  setProperty :: LoxInstance -> Identifier -> Value -> LoxAction ()
  setProperty obj prop val = liftIO $ modifyIORef (properties obj) $ Map.insert prop val

data Value
  = LitString String
  | LitNumber Double
  | LitBoolean Bool
  | Nil
  | Class LoxClass
  | forall t. (LoxObject t) => Object t
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
  show (Object obj) = show obj
  show (Class c) = show c

instance Eq Value where
  (==) :: Value -> Value -> Bool
  (LitBoolean b1) == (LitBoolean b2) = b1 == b2
  (LitNumber n1) == (LitNumber n2) = n1 == n2
  (LitString s1) == (LitString s2) = s1 == s2
  Nil == Nil = True
  _ == _ = False

isTruthy :: Value -> Bool
isTruthy Nil = False
isTruthy (LitBoolean b) = b
isTruthy _ = True

runLoxAction :: LoxAction a -> ProgramState -> IO (Either ShortCircuit a, ProgramState)
runLoxAction = runStateT . runExceptT

loxThrow :: String -> LoxAction a
loxThrow errMsg = do
    line <- gets lineNumber
    throwE $ Errored (line, errMsg)

loxCatch :: LoxAction a -> (LoxError -> LoxAction a) -> LoxAction a
loxCatch action handler = catchE action handler'
  where
    handler' (Errored err) = handler err
    handler' (Returned val) = loxReturn val

loxReturn :: Value -> LoxAction a
loxReturn = throwE . Returned

acceptReturn :: LoxAction a -> LoxAction Value
acceptReturn action = do
  catchE (action >> return Nil) handler
  where
    handler (Returned val) = return val
    handler err = throwE err

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ mb ma = do
  b <- mb
  when b $ ma >> whileM_ mb ma

reportError :: LoxError -> IO ()
reportError (ln, err) = do
  let errorBanner = "\027[1;31mERROR on line " ++ show ln ++ ": \027[22m"
  putStrLn $ errorBanner ++ err ++ "\027[0m"
