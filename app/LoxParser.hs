{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module LoxParser where

import LoxAST
import ParserCombinators
import Control.Applicative ((<|>), Alternative (some))

pBool :: Parser Bool
pBool = (match "true" >> return True) 
    <|> (match "false" >> return False)
    <|> fail "Invalid boolean value. Expected \"true\" or \"false\""

pNumber :: Parser Double
pNumber = do
    whole <- some digit
    frac <- ((:) <$> mchar '.' <*> some digit) <|> return ""
    return $ read $ whole ++ frac

pString :: Parser String
pString = do
    mchar '"'
    str <- consumeWhile (/= '\"')
    mchar '\"' <|> fail "Unterminated string"
    return str

pLiteral :: Parser Literal
pLiteral = LitBoolean <$> pBool
    <|> LitNumber <$> pNumber
    <|> LitString <$> pString
    <|> (match "nil" >> return Nil)

