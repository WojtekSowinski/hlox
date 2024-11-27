module OOP where

import Data.Map qualified as Map
import Functions (LoxCallable)
import LoxInternals (LoxAction, LoxCallable (..), LoxObject (..), Value)
import Scope (Identifier)
import Data.IORef (IORef, modifyIORef)
import Control.Monad.Trans (liftIO)

data LoxClass = LoxClass {name :: Identifier, superclass :: LoxClass}

instance Show LoxClass where
  show :: LoxClass -> String
  show c = "<class " ++ name c ++ ">"

instance LoxCallable LoxClass where
  call :: LoxClass -> [Value] -> LoxAction Value
  call = undefined -- TODO: object initialization
  arity :: LoxClass -> Int
  arity _ = 0

data LoxInstance = LoxInstance {klass :: LoxClass, properties :: IORef (Map.Map Identifier Value)}

instance Show LoxInstance where
  show :: LoxInstance -> String
  show obj = "<instnace of class " ++ name (klass obj) ++ ">"

instance LoxObject LoxInstance where
  getProperty :: LoxInstance -> Identifier -> LoxAction (Maybe Value)
  getProperty obj prop = Map.lookup prop (properties obj)
  setProperty :: LoxInstance -> Identifier -> Value -> LoxAction ()
  setProperty obj prop val = liftIO $ modifyIORef (properties obj) $ Map.insert prop val
