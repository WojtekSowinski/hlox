{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where

import Control.Monad (when)
import Control.Monad.State (MonadIO (liftIO))
import GHC.IO.Handle (hFlush, isEOF)
import GHC.IO.Handle.FD (stdout)
import LoxAST (Program)
import LoxInterpreter (exec, initialize)
import LoxParser (program, repl)
import ParserCombinators (ParseOutput (Matched), Parser (runParser))
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitSuccess, exitWith)
import LoxInternals (LoxAction, runLoxAction, ProgramState)

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
runRepl state = do
  code <- liftIO readPromptLine
  (Right exitCode, newState) <- runLoxAction (run code repl) state
  case exitCode of 
    ExitSuccess -> runRepl newState
    ExitFailure _ -> runRepl state

main :: IO ()
main = do
  args <- getArgs
  initState <- initialize
  case args of
    [] -> runRepl initState
    [filename] -> do
      code <- readFile filename
      (Right exitCode, _) <- runLoxAction (run code program) initState
      exitWith exitCode
    _ -> fail "Usage: hlox [script]"
