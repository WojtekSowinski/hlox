module OOP where

import Control.Monad.Trans (liftIO)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map qualified as Map
import Environment (Identifier)
import Functions (LoxCallable)
import LoxInternals (LoxAction, LoxCallable (..), LoxObject (..), Value (Object), loxThrow)

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
