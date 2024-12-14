module Scope where
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Control.Applicative ((<|>))

data Scope v = Scope {local :: Map.Map Identifier v, enclosing :: Maybe (Scope v)}

type Identifier = String

emptyScope :: Scope v
emptyScope = Scope {local = Map.empty, enclosing = Nothing}

fromList :: [(Identifier, v)] -> Scope v
fromList vals = Scope {local = Map.fromList vals, enclosing = Nothing}

isDefined :: Identifier -> Scope v -> Bool
isDefined varName = isJust . lookUp varName

lookUp :: Identifier -> Scope v -> Maybe v
lookUp varName scope =
  Map.lookup varName (local scope) <|> (lookUp varName =<< enclosing scope)

insertVar :: Identifier -> v -> Scope v -> Scope v
insertVar varName val scope =
  scope {local = Map.insert varName val $ local scope}

updateVar :: Identifier -> v -> Scope v -> Scope v
updateVar varName val scope =
  if Map.member varName (local scope)
    then insertVar varName val scope
    else scope {enclosing = updateVar varName val <$> enclosing scope}

enterNewScope :: [(Identifier, v)] -> Scope v -> Scope v
enterNewScope newVars scope = Scope {local = Map.fromList newVars, enclosing = Just scope}

discardLocal :: Scope v -> Scope v
discardLocal (Scope _ (Just enclosing)) = enclosing
discardLocal globalScope = globalScope
