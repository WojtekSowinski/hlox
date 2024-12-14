module Environment where

import Control.Applicative ((<|>))
import Data.Map qualified as Map
import Data.Maybe (isJust)

data Environment v = Environment {local :: Map.Map Identifier v, enclosing :: Maybe (Environment v)}

type Identifier = String

empty :: Environment v
empty = Environment {local = Map.empty, enclosing = Nothing}

fromList :: [(Identifier, v)] -> Environment v
fromList vals = Environment {local = Map.fromList vals, enclosing = Nothing}

isDefined :: Identifier -> Environment v -> Bool
isDefined varName = isJust . lookUp varName

lookUpLocal :: Identifier -> Environment v -> Maybe v
lookUpLocal varName scope = Map.lookup varName (local scope)

lookUp :: Identifier -> Environment v -> Maybe v
lookUp varName scope =
  lookUpLocal varName scope <|> (lookUp varName =<< enclosing scope)

insert :: Identifier -> v -> Environment v -> Environment v
insert varName val scope =
  scope {local = Map.insert varName val $ local scope}

update :: Identifier -> v -> Environment v -> Environment v
update varName val scope =
  if Map.member varName (local scope)
    then insert varName val scope
    else scope {enclosing = update varName val <$> enclosing scope}

initNewScope :: [(Identifier, v)] -> Environment v -> Environment v
initNewScope newVars scope = Environment {local = Map.fromList newVars, enclosing = Just scope}

discardLocal :: Environment v -> Environment v
discardLocal (Environment _ (Just enclosing)) = enclosing
discardLocal globalScope = globalScope
