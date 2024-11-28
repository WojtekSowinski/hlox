module Functions (clock) where

import LoxInternals (LoxAction, LoxCallable (..), Value (LitNumber))
import Control.Monad.Trans (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)

data ForeignFunction = ForeignFunction
  { name :: String,
    apply :: [Value] -> LoxAction Value,
    numParams :: Int
  }

instance Show ForeignFunction where
  show f = "<foreign fn " ++ name f ++ ">"

instance LoxCallable ForeignFunction where
  call :: ForeignFunction -> [Value] -> LoxAction Value
  call = apply
  arity :: ForeignFunction -> Int
  arity = numParams

clock :: ForeignFunction
clock = ForeignFunction {
    name = "clock",
    apply = \_ -> liftIO $ LitNumber . fromInteger . round <$> getPOSIXTime,
    numParams = 0}
