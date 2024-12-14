module StaticAnalysis (errorsIn) where

import Control.Monad.State (State, evalState, get, modify, put)
import Environment (Identifier)
import LoxAST (Expression (..), FunctionDef (..), Program, Statement (..))
import LoxInternals (LoxError)

errorsIn :: Program -> [LoxError]
errorsIn = concatMap $ errorsInStatement TopLevel

data Context = TopLevel | Function | Method ClassType | Initializer ClassType
data ClassType = BaseClass | SubClass

errorsInStatement :: Context -> Statement -> [LoxError]
errorsInStatement _ NOP =
  []
errorsInStatement _ (CouldNotParse ln err) =
  [(ln, "Parse error - " ++ err)]
errorsInStatement ctx (Eval expr) =
  errorsInExpression ctx expr
errorsInStatement ctx (Print expr) =
  errorsInExpression ctx expr
errorsInStatement ctx (If cond trueBranch falseBranch) =
  errorsInExpression ctx cond
    ++ errorsInStatement ctx trueBranch
    ++ errorsInStatement ctx falseBranch
errorsInStatement ctx (While cond body) =
  errorsInExpression ctx cond ++ errorsInStatement ctx body
errorsInStatement TopLevel (FunctionDecl f) =
  errorsInFunction Function f
errorsInStatement ctx (FunctionDecl f) =
  errorsInFunction ctx f
errorsInStatement TopLevel (Return line expr) =
  (line, "Can't return from outside a function.")
    : maybe [] (errorsInExpression TopLevel) expr
errorsInStatement _ (Return _ Nothing) =
  []
errorsInStatement ctx@(Initializer _) (Return line (Just expr)) =
  (line, "Can't return a value from an initializer.")
    : errorsInExpression ctx expr
errorsInStatement ctx (Return _ (Just expr)) =
  errorsInExpression ctx expr
errorsInStatement ctx block@(Block statements) =
  evalState (doubleDeclarations block) []
    ++ concatMap (errorsInStatement ctx) statements
errorsInStatement ctx (ClassDecl _ _ super methods) =
  case super of
    Nothing -> errorsInClass BaseClass methods
    Just expr -> errorsInExpression ctx expr ++ errorsInClass SubClass methods
errorsInStatement ctx (VarInitialize line name expr) =
  [(line, "Declaration of variable " ++ name ++ " refers to itself.") | expr `refersTo` name]
    ++ errorsInExpression ctx expr

errorsInFunction :: Context -> FunctionDef -> [LoxError]
errorsInFunction ctx (FunctionDef _ _ _ body) =
  errorsInStatement ctx body
errorsInFunction _ (TooManyParams line) =
  [(line, "Function can't have more than 255 parameters.")]
errorsInFunction _ (DuplicateParams line) =
  [(line, "Function definition assigns the same identifier to multiple parameters.")]

errorsInExpression :: Context -> Expression -> [LoxError]
errorsInExpression _ (TooManyArgs line) =
  [(line, "Function call can't take more than 255 arguments.")]
errorsInExpression _ (InvalidAssignmentTarget line) =
  [(line, "Invalid assignment target.")]
errorsInExpression ctx (Negative _ operand) =
  errorsInExpression ctx operand
errorsInExpression ctx (Not operand) =
  errorsInExpression ctx operand
errorsInExpression ctx (BinOperation _ left _ right) =
  errorsInExpression ctx left ++ errorsInExpression ctx right
errorsInExpression ctx (And left right) =
  errorsInExpression ctx left ++ errorsInExpression ctx right
errorsInExpression ctx (Or left right) =
  errorsInExpression ctx left ++ errorsInExpression ctx right
errorsInExpression ctx (Assign left right) =
  errorsInExpression ctx left ++ errorsInExpression ctx right
errorsInExpression ctx (FunctionCall _ f args) =
  errorsInExpression ctx f ++ concatMap (errorsInExpression ctx) args
errorsInExpression ctx (AccessProperty _ object _) =
  errorsInExpression ctx object
errorsInExpression TopLevel (This line) =
  [(line, "Can't reference 'this' outside of a class definition.")]
errorsInExpression Function (This line) =
  [(line, "Can't reference 'this' outside of a class definition.")]
errorsInExpression (Initializer classType) super@(Super _ _) =
  errorsInExpression (Method classType) super
errorsInExpression (Method BaseClass) (Super line _) =
  [(line, "Can't use 'super' in a class with no superclass")]
errorsInExpression TopLevel (Super line _) =
  [(line, "Can't use 'super' outside of a class.")]
errorsInExpression Function (Super line _) =
  [(line, "Can't use 'super' outside of a class.")]
errorsInExpression _ _ = []

refersTo :: Expression -> Identifier -> Bool
refersTo (Variable _ var) name =
  var == name
refersTo (BinOperation _ left _ right) name =
  (left `refersTo` name) || (right `refersTo` name)
refersTo (And left right) name =
  (left `refersTo` name) || (right `refersTo` name)
refersTo (Or left right) name =
  (left `refersTo` name) || (right `refersTo` name)
refersTo (Assign left right) name =
  (left `refersTo` name) || (right `refersTo` name)
refersTo (Negative _ expr) name =
  expr `refersTo` name
refersTo (Not expr) name =
  expr `refersTo` name
refersTo (FunctionCall _ fn args) name =
  any (`refersTo` name) (fn : args)
refersTo (AccessProperty _ object _) name =
  object `refersTo` name
refersTo _ _ = False

doubleDeclarations :: Statement -> State [Identifier] [LoxError]
doubleDeclarations (VarInitialize line name _) = do
  found <- get
  if name `elem` found
    then return [(line, "Variable " ++ name ++ " declared multiple times in the same scope.")]
    else modify (name :) >> return []
doubleDeclarations (Block statements) = do
  enclosing <- get
  put []
  errors <- concat <$> mapM doubleDeclarations statements
  put enclosing
  return errors
doubleDeclarations _ = return []

errorsInClass :: ClassType -> [FunctionDef] -> [LoxError]
errorsInClass classType = helper [] where
  helper :: [Identifier] -> [FunctionDef] -> [LoxError]
  helper _ [] =
    []
  helper found (def@(FunctionDef line name _ _) : ms) =
    [(line, "Method '" ++ name ++ "' defined multiple times in the same class.") | name `elem` found]
      ++ errorsInFunction (chooseContext name) def
      ++ helper (name : found) ms
  helper found (def : ms) =
    errorsInFunction (chooseContext "foo") def
      ++ helper found ms
  chooseContext :: Identifier -> Context
  chooseContext name = (if name == "init" then Initializer else Method) classType

