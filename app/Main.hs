{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Monad (void, when)
import Control.Monad.State (MonadIO (liftIO))
import Exec (LoxAction, ProgramState (..), exec, runLoxAction)
import GHC.IO.Handle (hFlush, isEOF)
import GHC.IO.Handle.FD (stdout)
import LoxParser (pProgram)
import ParserCombinators (ParseOutput (Matched), Parser (runParser))
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import qualified Data.Map as Map

run :: String -> LoxAction ()
run code = do
  let (_, Matched statements) = runParser pProgram (code, 1)
  mapM_ exec statements

readPromptLine :: IO String
readPromptLine = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  when eof exitSuccess
  getLine

runPrompt :: ProgramState -> IO ()
runPrompt st = do
  code <- liftIO readPromptLine
  newState <- runLoxAction (run code) st
  runPrompt newState

initState :: ProgramState
initState = ProgramState {
    exitOnError = True,
    environment = Map.empty
    }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runPrompt $ initState {exitOnError = False}
    [filename] -> do
      code <- readFile filename
      void $ runLoxAction (run code) initState
    _ -> fail "Usage: hlox [script]"
