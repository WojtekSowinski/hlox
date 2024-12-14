module LoxInternals
  ( ProgramState (..),
    LoxAction,
    LoxError,
    LoxCallable (..),
    LoxFunction (..),
    LoxClass (..),
    LoxInstance (..),
    Value (..),
    define,
    enterNewScope,
    exitScope,
    setLine,
    getProperty,
    setProperty,
    isTruthy,
    runLoxAction,
    loxThrow,
    loxCatch,
    loxReturn,
    acceptReturn,
    reportError
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (StateT, get, gets, modify, put, runStateT)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (catchE, throwE)
import Data.Char (toLower)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map qualified as Map
import Environment

data ProgramState = ProgramState
  { env :: Environment (IORef Value),
    lineNumber :: Int,
    this :: Value
  }

type LoxAction = ExceptT ShortCircuit (StateT ProgramState IO)

type LoxError = (Int, String)

data ShortCircuit = Errored LoxError | Returned Value

define :: Identifier -> Value -> LoxAction (IORef Value)
define name value = do
  ref <- liftIO $ newIORef value
  modify (\s -> s {env = insert name ref (env s)})
  return ref

enterNewScope :: [(Identifier, Value)] -> LoxAction ()
enterNewScope dict = do
  let (names, vals) = unzip dict
  refs <- mapM (liftIO . newIORef) vals
  modify (\s -> s {env = initNewScope (zip names refs) (env s)})

exitScope :: LoxAction ()
exitScope = modify (\s -> s {env = discardLocal (env s)})

setLine :: Int -> LoxAction ()
setLine n = modify (\s -> s {lineNumber = n})

class (Show f) => LoxCallable f where
  call :: f -> [Value] -> LoxAction Value
  arity :: f -> Int

data LoxClass = LoxClass
  { name :: Identifier,
    superclass :: Maybe LoxClass,
    methods :: [(Identifier, LoxFunction)]
  }

findMethod :: Identifier -> LoxClass -> Maybe LoxFunction
findMethod name LoxClass {methods = methods, superclass=super} = 
    lookup name methods <|> (findMethod name =<< super) 

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

data LoxFunction = LoxFunction
  { fnName :: Identifier,
    fnParams :: [Identifier],
    fnBody :: LoxAction (),
    closure :: ProgramState
  }

instance Show LoxFunction where
  show f = "<fn " ++ fnName f ++ ">"

instance LoxCallable LoxFunction where
  call :: LoxFunction -> [Value] -> LoxAction Value
  call f args = do
    oldState <- get
    put $ closure f
    enterNewScope $ zip (fnParams f) args
    returnVal <- acceptReturn $ fnBody f
    put oldState
    return returnVal

  arity :: LoxFunction -> Int
  arity = length . fnParams

data LoxInstance = LoxInstance
  { klass :: LoxClass,
    properties :: IORef (Map.Map Identifier Value)
  }

instance Show LoxInstance where
  show :: LoxInstance -> String
  show obj = "<instnace of class " ++ name (klass obj) ++ ">"

instance Eq LoxInstance where
  (==) :: LoxInstance -> LoxInstance -> Bool
  x == y = properties x == properties y

getProperty :: LoxInstance -> Identifier -> LoxAction Value
getProperty obj prop = do
  props <- liftIO $ readIORef $ properties obj
  case Map.lookup prop props <|> (Function . bind obj <$> findMethod prop (klass obj)) of
    Just val -> return val
    Nothing -> loxThrow ("Undefined property '" ++ prop ++ "'.")

bind :: LoxInstance -> LoxFunction -> LoxFunction
bind obj f@LoxFunction {closure = c} = f {closure = c {this = Object obj}}

setProperty :: LoxInstance -> Identifier -> Value -> LoxAction ()
setProperty obj prop val = liftIO $ modifyIORef (properties obj) $ Map.insert prop val

data Value
  = LitString String
  | LitNumber Double
  | LitBoolean Bool
  | Nil
  | Class LoxClass
  | Object LoxInstance
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
  (Object x) == (Object y) = x == y
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

reportError :: LoxError -> IO ()
reportError (ln, err) = do
  let errorBanner = "\027[1;31mERROR on line " ++ show ln ++ ": \027[22m"
  putStrLn $ errorBanner ++ err ++ "\027[0m"
