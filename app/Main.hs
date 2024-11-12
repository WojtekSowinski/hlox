{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.State (MonadIO (liftIO))
import GHC.IO.Handle (hFlush, isEOF)
import GHC.IO.Handle.FD (stdout)
import LoxAST (Program)
import LoxInterpreter (LoxAction, ProgramState, exec, initState, runLoxAction)
import LoxParser (program, repl)
import ParserCombinators (ParseOutput (Matched), Parser (runParser))
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

run :: String -> Parser Program -> LoxAction ExitCode
run code parser = do
  case runParser parser (code, 1) of
    (_, Matched statements) -> exec statements
    _ -> liftIO (putStrLn "PARSER BUG!!!") >> return (ExitFailure 1)

readPromptLine :: IO String
readPromptLine = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  when eof exitSuccess
  getLine

runRepl :: ProgramState -> IO ()
runRepl st = do
  code <- liftIO readPromptLine
  (_, newState) <- runLoxAction (run code repl) st
  runRepl newState

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl initState
    [filename] -> do
      code <- readFile filename
      (exitCode, _) <- runLoxAction (run code program) initState
      exitWith exitCode
    _ -> fail "Usage: hlox [script]"
