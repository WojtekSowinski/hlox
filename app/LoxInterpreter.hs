{-# LANGUAGE DuplicateRecordFields #-}

module LoxInterpreter (exec, initialize) where

import Control.Monad.State
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Environment
import Functions (clock)
import LoxAST
import LoxInternals
import StaticAnalysis
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Prelude hiding (EQ, GT, LT)

initialize :: IO ProgramState
initialize = do
  clockRef <- newIORef $ Function clock
  return $
    ProgramState
      { lineNumber = 1,
        env = fromList [("clock", clockRef)],
        this = Nil,
        super = undefined
      }

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
interpret (Eval ex) = void $ eval ex
interpret (Print ex) = eval ex >>= (liftIO . print)
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
interpret (FunctionDecl f) = void $ makeTopLevelFunction f
interpret (ClassDecl line name super methods) =
  setLine line >> createClass name super methods
interpret (Return _ (Just ex)) = do
  retVal <- eval ex
  loxReturn retVal
interpret (Return _ Nothing) = loxReturn Nil
interpret (CouldNotParse _ _) = undefined

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ mb ma = do
  b <- mb
  when b $ ma >> whileM_ mb ma

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
eval (This _) = gets this
eval (Super line method) = do
  Object obj <- gets this
  setLine line
  klass <- gets super
  case findMethod method klass of
    Nothing -> loxThrow ("Undefined method '" ++ method ++ "'.")
    Just f -> return $ Function $ bind obj f
eval (Assign _ target ex) = assign target ex
eval (FunctionCall line funcEx argExs) = do
  func <- eval funcEx
  args <- mapM eval argExs
  setLine line
  case func of
    Function f -> checkArity f args >> call f args
    Class c -> checkArity c args >> call c args
    _ -> loxThrow ("Can't call " ++ show func ++ " as it is not a function.")
eval (AccessProperty line ex prop) = do
  operand <- eval ex
  case operand of
    Object obj -> setLine line >> getProperty obj prop
    _ -> loxThrow ("Can't access properties on " ++ show operand ++ " as it is not an object.")

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
    Object obj -> (setProperty obj prop =<< eval ex) >> return object
    _ -> loxThrow ("Can't set properties on " ++ show object ++ " as it is not an object.")
assign _ _ = undefined

getLocalRef :: Identifier -> LoxAction (IORef Value)
getLocalRef name = do
  oldState <- gets env
  case lookUpLocal name oldState of
    Just ref -> return ref
    Nothing -> define name undefined

makeTopLevelFunction :: FunctionDef -> LoxAction LoxFunction
makeTopLevelFunction FunctionDef {body=body, params=params, name=name} = do
  funcRef <- getLocalRef name
  currentState <- get
  let f =
        LoxFunction
          { fnName = name,
            fnParams = params,
            fnBody = interpret body,
            closure = currentState
          }
  liftIO $ writeIORef funcRef $ Function f
  return f

makeMethod :: ProgramState -> FunctionDef -> (Identifier, LoxFunction)
makeMethod closure FunctionDef {body=body, params=params, name=name} = (name, fn)
  where
    fnBody =
      if name == "init"
        then acceptReturn (interpret body) >> (gets this >>= loxReturn)
        else interpret body
    fn = LoxFunction {fnParams = params, fnName = name, fnBody = fnBody, closure = closure}

createClass :: Identifier -> Maybe Expression -> [FunctionDef] -> LoxAction ()
createClass className superExpr methodDefs = do
  superclass <- evalSuperclass superExpr
  currentState <- get
  let methodClosure =
        ( case superclass of
            Nothing -> currentState
            Just klass -> currentState {super = klass}
        )
  let methods = map (makeMethod methodClosure) methodDefs
  ref <- getLocalRef className
  liftIO $ writeIORef ref $ Class $ LoxClass className superclass methods

evalSuperclass :: Maybe Expression -> LoxAction (Maybe LoxClass)
evalSuperclass Nothing = return Nothing
evalSuperclass (Just expr) = do
  super <- eval expr
  case super of
    Class c -> return $ Just c
    _ -> loxThrow "Superclass must be a class."
