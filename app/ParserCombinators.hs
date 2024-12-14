module ParserCombinators where

import Control.Applicative (Alternative (empty), (<|>))
import Control.Monad (unless, MonadPlus)
import Data.Foldable (foldl')

type ParseError = String

type ParseState = (String, Int)

newtype Parser t = Parser {runParser :: ParseState -> Either ParseError (ParseState, t)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ Right . (,a)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) = Parser p
    where
      p st = case p1 st of
        Left err -> Left err
        Right (st', f) -> (fmap $ fmap f) (p2 st')

instance Alternative Parser where
  empty :: Parser a
  empty = fail "No viable input detected."
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser p
    where
      p st = case p1 st of
        out@(Right _) -> out
        Left _ -> p2 st

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser pa) >>= f = Parser p
    where
      p st = case pa st of
        Left err -> Left err
        Right (st', a) -> runParser (f a) st'

instance MonadFail Parser where
  fail :: String -> Parser a
  fail err = Parser (\(_, ln) -> Left $ errOnLine ln err)

instance MonadPlus Parser

choice :: Alternative f => [f a] -> f a
choice = foldl' (<|>) empty

errOnLine :: Int -> ParseError -> ParseError
errOnLine ln err = "Error on line " ++ show ln ++ ['\n'] ++ err

consumeChar :: Parser Char
consumeChar = Parser p
  where
    p ('\n' : cs, ln) = Right ((cs, ln + 1), '\n')
    p (c : cs, ln) = Right ((cs, ln), c)
    p ("", ln) = Left $ errOnLine ln "Unexpected end of input"

eof :: Parser ()
eof = Parser p
  where
    p st@("", _) = Right (st, ())
    p (c : _, ln) = Left $ errOnLine ln "Expected EOF, found " ++ show c

mchar :: Char -> Parser Char
mchar expected = Parser p
  where
    p ("", ln) = Left $ errOnLine ln "Unexpected end of input"
    p (c : cs, ln)
      | c /= expected = Left $ errOnLine ln $ "Expected " ++ show expected ++ ", found " ++ show c
      | c == '\n' = Right ((cs, ln + 1), '\n')
      | otherwise = Right ((cs, ln), c)

match :: String -> Parser String
match s = mapM mchar s <|> fail ("Expected " ++ show s)

consumeWhile :: (Char -> Bool) -> Parser String
consumeWhile f = Parser p where
    p (input, ln) = Right ((remainder, newLn), consumed) where
        (consumed, remainder) = span f input
        newLn = count '\n' consumed + ln
        count x (y:ys) 
            | x /= y = count x ys
            | otherwise = count x ys + 1
        count _ [] = 0