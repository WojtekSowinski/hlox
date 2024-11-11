{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Monad (void, when)
import Control.Monad.State (MonadIO (liftIO))
import Data.Map qualified as Map
import LoxInterpreter (LoxAction, ProgramState (..), exec, runLoxAction)
import GHC.IO.Handle (hFlush, isEOF)
import GHC.IO.Handle.FD (stdout)
import LoxParser (pProgram, pRepl)
import ParserCombinators (ParseOutput (Matched), Parser (runParser))
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import LoxAST (Program)

run :: String -> Parser Program -> LoxAction ()
run code parser = do
  let (_, Matched statements) = runParser parser (code, 1)
  exec statements

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
  newState <- runLoxAction (run code pRepl) st
  runRepl newState

initState :: ProgramState
initState =
  ProgramState
    { exitOnError = True,
      environment = Map.empty
    }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runRepl $ initState {exitOnError = False}
    [filename] -> do
      code <- readFile filename
      void $ runLoxAction (run code pProgram) initState
    _ -> fail "Usage: hlox [script]"
