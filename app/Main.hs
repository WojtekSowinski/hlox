module Main where
import System.Environment (getArgs)
import GHC.IO.Handle (isEOF, hFlush)
import Control.Monad (when)
import System.Exit (exitSuccess)
import GHC.IO.Handle.FD (stdout)

run :: String -> IO ()
run = undefined

runPrompt :: IO ()
runPrompt = do
    putStr "> "
    hFlush stdout
    eof <- isEOF
    when eof exitSuccess
    getLine >>= run
    runPrompt

dispatchArgs :: [String] -> IO ()
dispatchArgs [] = runPrompt
dispatchArgs [filename] = readFile filename >>= run
dispatchArgs _ = error "Usage: hlox [script]"

main :: IO ()
main = getArgs >>= dispatchArgs
