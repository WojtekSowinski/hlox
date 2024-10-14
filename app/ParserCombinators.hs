module ParserCombinators where

import Control.Applicative (Alternative (empty), (<|>))
import Control.Monad (MonadPlus, mfilter)
import Data.Foldable (foldl')
import Data.List (isPrefixOf)

type ParseError = String

type ParseState = (String, Int)

data ParseOutput a
  = Matched a -- The input was parsed successfully
  | Failed ParseError -- The input didn't match the expected pattern. Try other parsers or panic
  | Panicked ParseError -- Error!!! Short-circuit other parsers, enter panic mode, report the error and synchronize
  deriving (Functor, Show)

newtype Parser a = Parser {runParser :: ParseState -> (ParseState, ParseOutput a)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (,Matched a)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> p2 = Parser p
    where
      p st =
        case p1 st of
          (st', Matched f) -> runParser (fmap f p2) st'
          (st', Failed err) -> (st', Failed err)
          (st', Panicked err) -> (st', Panicked err)

instance Alternative Parser where
  empty :: Parser a
  empty = fail "Input didn't match any expected pattern."
  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser p
    where
      p st = case p1 st of
        (_, Failed _) -> p2 st
        matchOrPanic -> matchOrPanic

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser pa) >>= f = Parser p
    where
      p st = case pa st of
        (st', Matched a) -> runParser (f a) st'
        (st', Failed err) -> (st', Failed err)
        (st', Panicked err) -> (st', Panicked err)

instance MonadFail Parser where
  fail :: String -> Parser a
  fail err = Parser (,Failed err)

-- | A parser that enters panic mode.
--  Use to signal the need to stop parsing, report an error and synchronize
panic :: ParseError -> Parser a
panic err = Parser (,Panicked err)

-- | Recover from a panic or unhandled failure
(<-!!!->) :: Parser a -> (ParseError -> Parser a) -> Parser a
Parser pa <-!!!-> sync = Parser p
  where
    p st = case pa st of
      success@(_, Matched _) -> success
      (st', Panicked err) -> runParser (sync err) st'
      (st', Failed err) -> runParser (sync $ "Unhandled parser failure: " ++ err) st'

infixl 3 <-!!!-> -- <-!!!-> has the same precedence and associativity as <|>

instance MonadPlus Parser

choice :: (Alternative f) => [f a] -> f a
choice = foldl' (<|>) empty

consumeChar :: Parser Char
consumeChar = Parser p
  where
    p (c : cs, ln) = ((cs, if c == '\n' then ln + 1 else ln), Matched c)
    p st@("", _) = (st, Failed "Unexpected end of file.")

eof :: Parser ()
eof = Parser p
  where
    p st@("", _) = (st, Matched ())
    p st = (st, Failed "Expected end of file.")

mchar :: Char -> Parser Char
mchar expected = mfilter (== expected) consumeChar

count :: (Eq a) => a -> [a] -> Int
count = count' 0
  where
    count' acc _ [] = acc
    count' acc x (y : ys) = count' (acc + if x == y then 1 else 0) x ys

match :: String -> Parser String
match str = Parser p
  where
    p st@(input, ln)
      | str `isPrefixOf` input = ((remaining, newLn), Matched str)
      | otherwise = (st, Failed $ "Expected " ++ show str)
      where
        remaining = drop (length str) input
        newLn = ln + count '\n' str

consumeWhile :: (Char -> Bool) -> Parser String
consumeWhile f = Parser p
  where
    p (input, ln) = ((remaining, newLn), Matched consumed)
      where
        (consumed, remaining) = span f input
        newLn = count '\n' consumed + ln
