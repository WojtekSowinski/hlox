module LoxInterpreter
  ( LoxAction,
    ProgramState,
    exec,
    initialize,
    runLoxAction,
  )
where

import Control.Monad.State
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Functions (clock)
import LoxAST
import LoxInternals
import Environment
import StaticAnalysis (errorsIn)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Prelude hiding (EQ, GT, LT)

initialize :: IO ProgramState
initialize = do
  clockRef <- newIORef $ Function clock
  return $ ProgramState {lineNumber = 1, env = fromList [("clock", clockRef)]}

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

exec :: Program -> LoxAction ExitCode
exec program = do
  let errors = errorsIn program
  if null errors
    then
      loxCatch
        (mapM_ interpret program >> return ExitSuccess)
        (\err -> liftIO (reportError err) >> return (ExitFailure 1))
    else liftIO (mapM_ reportError errors) >> return (ExitFailure 1)

interpret :: Statement -> LoxAction ()
interpret NOP = return ()
interpret (Eval ex) =
  loxCatch
    (void $ eval ex)
    (\(_, err) -> loxThrow $ "Couldn't evaluate expression - " ++ err)
interpret (Print ex) =
  loxCatch
    (eval ex >>= (liftIO . print))
    (\(_, err) -> loxThrow $ "Couldn't evaluate expression - " ++ err)
interpret (VarInitialize _ varName ex) = do
  initVal <- eval ex
  void $ define varName initVal
interpret (Block statements) = do
  enterNewScope []
  mapM_ interpret statements
  exitScope
interpret (If cond trueBranch falseBranch) = do
  condVal <- eval cond
  interpret $ if isTruthy condVal then trueBranch else falseBranch
interpret (While cond body) = whileM_ (isTruthy <$> eval cond) (interpret body)
interpret (FunctionDecl f) = createFunction f
interpret (ClassDecl name methods) = createClass name methods
interpret (Return _ (Just ex)) = do
  retVal <- eval ex
  loxReturn retVal
interpret (Return _ Nothing) = loxReturn Nil
interpret (CouldNotParse _ _) = undefined

eval :: Expression -> LoxAction Value
eval (Literal val) = return val
eval (Not ex) = LitBoolean . not . isTruthy <$> eval ex
eval (Negative line ex) = do
  operand <- eval ex
  setLine line
  case operand of
    LitNumber val -> return $ LitNumber (-val)
    _ -> loxThrow "Operand of a negation must be a number."
eval (And leftEx rightEx) = do
  leftOp <- eval leftEx
  if isTruthy leftOp then eval rightEx else return leftOp
eval (Or leftEx rightEx) = do
  leftOp <- eval leftEx
  if not $ isTruthy leftOp then eval rightEx else return leftOp
eval (BinOperation line leftEx op rightEx) = do
  leftOp <- eval leftEx
  rightOp <- eval rightEx
  setLine line
  applyBinOp leftOp op rightOp
eval (Variable line varName) = do
  scope <- gets env
  setLine line
  case lookUp varName scope of
    Nothing -> loxThrow ("Undefined Variable " ++ show varName ++ ".")
    Just ref -> liftIO $ readIORef ref
eval (Assign _ target ex) = assign target ex
eval (FunctionCall line funcEx argExs) = do
  func <- eval funcEx
  args <- mapM eval argExs
  setLine line
  case func of
    Function f -> checkArity f args >> call f args
    _ -> loxThrow ("Can't call " ++ show func ++ " as it is not a function.")
eval (AccessProperty line ex prop) = do
  operand <- eval ex
  case operand of
    Object obj -> setLine line >> getProperty obj prop
    _ -> loxThrow ("Can't access properties on " ++ show operand ++ " as it is not an object.")
eval (TooManyArgs _) = undefined
eval (InvalidAssignmentTarget _) = undefined

applyBinOp :: Value -> BinOp -> Value -> LoxAction Value
applyBinOp left EQ right = return $ LitBoolean $ left == right
applyBinOp left NEQ right = return $ LitBoolean $ left /= right
applyBinOp (LitString s1) ADD (LitString s2) = return $ LitString $ s1 ++ s2
applyBinOp (LitNumber n1) op (LitNumber n2) = return $ calc n1 op n2
applyBinOp _ ADD _ = loxThrow "Operands of '+' must be numbers or strings."
applyBinOp _ op _ = loxThrow ("Operands of '" ++ show op ++ "' must be numbers.")

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

checkArity :: (LoxCallable f) => f -> [Value] -> LoxAction ()
checkArity f args = do
  let argNum = length args
  let expected = arity f
  when (argNum /= expected) $
    loxThrow ("Expected " ++ show expected ++ " arguments, got " ++ show argNum ++ ".")

assign :: Expression -> Expression -> LoxAction Value
assign (Variable line varName) ex = do
  scope <- gets env
  setLine line
  case lookUp varName scope of
    Nothing -> loxThrow ("Undefined variable " ++ show varName ++ ".")
    Just ref -> do
      newVal <- eval ex
      liftIO $ writeIORef ref newVal
      return newVal
assign (AccessProperty line objExpr prop) ex = do
  object <- eval objExpr
  setLine line
  case object of
    Object obj -> (setProperty obj prop <$> eval ex) >> return object
    _ -> loxThrow ("Can't set properties on " ++ show object ++ " as it is not an object.")
assign _ _ = undefined

data LoxFunction = LoxFunction
  { fnName :: Identifier,
    fnParams :: [Identifier],
    fnBody :: Statement,
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
    returnVal <- acceptReturn $ interpret $ fnBody f
    put oldState
    return returnVal
  arity :: LoxFunction -> Int
  arity = length . fnParams

getLocalRef :: Identifier -> Value -> LoxAction (IORef Value)
getLocalRef name value = do
  oldState <- gets env
  case lookUpLocal name oldState of
    Just ref -> return ref
    Nothing -> define name value

createFunction :: FunctionDef -> LoxAction ()
createFunction (FunctionDef name params body) = do
  funcRef <- getLocalRef name undefined
  currentState <- get
  liftIO $
    writeIORef funcRef $
      Function
        LoxFunction
          { fnName = name,
            fnParams = params,
            fnBody = body,
            closure = currentState
          }
createFunction _ = undefined

createClass :: Identifier -> [FunctionDef] -> LoxAction ()
createClass = undefined
