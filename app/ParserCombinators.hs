module ParserCombinators
  ( ParseError,
    ParseState,
    Location (Location),
    ParseOutput (..),
    Parser (..),
    parse,
    getLocation,
    panic,
    calm,
    (<!>),
    consumeChar,
    eof,
    mchar,
    match,
    consumeWhile,
    findNext,
    testFor,
    untilP,
    sepBy,
    choice,
  )
where

import Control.Applicative (Alternative (empty, many), (<|>))
import Control.Monad (MonadPlus, mfilter)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.List (isPrefixOf)

type ParseError = String

type ParseState = (String, Location)

data Location = Location {line :: Int, column :: Int}

instance Show Location where
  show (Location line column) = show line ++ ':' : show column

data ParseOutput a
  = Matched a -- The input was parsed successfully
  | Failed ParseError -- The input didn't match the expected pattern. Try other parsers or panic
  | Panicked ParseError -- Error! Short-circuit other parsers, enter panic mode, report the error and synchronize
  deriving (Functor, Show)

newtype Parser a = Parser {runParser :: ParseState -> (ParseState, ParseOutput a)}

parse :: String -> Parser a -> (ParseState, ParseOutput a)
parse input parser = runParser parser (input, Location 1 1)

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

-- | Modify the supplied parser to fail instead of panicking
calm :: Parser a -> Parser a
calm (Parser pa) = Parser p
  where
    p st = case pa st of
      (st', Panicked err) -> (st', Failed err)
      result -> result

-- | Recover from a panic or unhandled failure
(<!>) :: Parser a -> (ParseError -> Parser a) -> Parser a
Parser pa <!> sync = Parser p
  where
    p st = case pa st of
      success@(_, Matched _) -> success
      (st', Panicked err) -> runParser (sync err) st'
      (st', Failed err) -> runParser (sync err) st'

infixl 3 <!> -- <!> has the same precedence and associativity as <|>

instance MonadPlus Parser

choice :: (Alternative f) => [f a] -> f a
choice = foldl' (<|>) empty

consumeChar :: Parser Char
consumeChar = Parser p
  where
    p (c : cs, Location ln col) = ((cs, newLocation), Matched c)
      where
        newLocation = if c == '\n' then Location (ln + 1) 1 else Location ln (col + 1)
    p st@("", _) = (st, Failed "Unexpected end of file.")

eof :: Parser ()
eof = Parser p
  where
    p st@("", _) = (st, Matched ())
    p st = (st, Failed "Expected end of file.")

mchar :: Char -> Parser Char
mchar expected = mfilter (== expected) consumeChar

advanceLocation :: Location -> String -> Location
advanceLocation loc [] = loc
advanceLocation (Location {line = l}) ('\n' : rest) = advanceLocation (Location (l + 1) 1) rest
advanceLocation loc@(Location {column = c}) (_ : rest) = advanceLocation (loc {column = c + 1}) rest

match :: String -> Parser String
match str = Parser p
  where
    p st@(input, loc)
      | str `isPrefixOf` input = ((remaining, newLoc), Matched str)
      | otherwise = (st, Failed $ "Expected " ++ show str)
      where
        remaining = drop (length str) input
        newLoc = advanceLocation loc str

consumeWhile :: (Char -> Bool) -> Parser String
consumeWhile f = Parser p
  where
    p (input, loc) = ((remaining, newLoc), Matched consumed)
      where
        (consumed, remaining) = span f input
        newLoc = advanceLocation loc consumed

getLocation :: Parser Location
getLocation = Parser (\st@(_, loc) -> (st, Matched loc))

findNext :: Parser a -> Parser a
findNext pa = Parser p
  where
    p s = recurse s s
    recurse st@(inp, _) initSt = case runParser (calm pa) st of
      out@(_, Matched _) -> out
      (_, err) ->
        if null inp
          then (initSt, err)
          else recurse (fst $ runParser consumeChar st) initSt

testFor :: Parser a -> Parser a
testFor pa = Parser p
  where
    p st = case runParser (calm pa) st of
      (_, Matched a) -> (st, Matched a)
      (_, err) -> (st, err)

untilP :: Parser a -> Parser b -> Parser [b]
untilP cond loop = (calm cond $> []) <|> ((:) <$> loop <*> untilP cond loop)

sepBy :: Parser a -> Parser s -> Parser [a]
sepBy pa ps = ((:) <$> pa <*> many (ps *> pa)) <|> return []
