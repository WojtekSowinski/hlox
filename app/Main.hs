{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main where

import Control.Monad (void, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (MonadIO (liftIO), StateT (runStateT))
import Exec (LoxAction, ProgramState (..), exec)
import GHC.IO.Handle (hFlush, isEOF)
import GHC.IO.Handle.FD (stdout)
import LoxParser (pStatement)
import ParserCombinators (ParseOutput (Matched), Parser (runParser))
import System.Environment (getArgs)
import System.Exit (exitSuccess)

run :: String -> LoxAction ()
run code = do
  let (_, Matched ast) = runParser pStatement (code, 1)
  exec ast

readPromptLine :: IO String
readPromptLine = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  when eof exitSuccess
  getLine

runPrompt :: LoxAction ()
runPrompt = do
  code <- liftIO readPromptLine
  run code
  runPrompt

initState :: ProgramState
initState = ProgramState {exitOnError = True}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> void $ runExceptT $ runStateT runPrompt $ initState {exitOnError = False}
    [filename] -> do
      code <- readFile filename
      void $ runExceptT $ runStateT (run code) initState
    _ -> fail "Usage: hlox [script]"
