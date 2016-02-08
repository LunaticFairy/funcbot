{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Control.Monad (liftM)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char

type MString = Maybe String
data IrcLine = IrcLine IrcPrefix String [String] MString
    deriving (Show)

data IrcPrefix = Server String
               | User String String String
    deriving (Show)

prefix :: Parser IrcPrefix
prefix = Server <$> (char ':' >> manyTill anyChar space)

command :: Parser String
command = manyTill anyChar (space)

params :: Parser [String]
params = words `liftM` manyTill anyChar (try (string " :" <|> string "\r"))

trail :: Parser String
trail = manyTill anyChar (char '\r')

irc :: Parser IrcLine
irc = IrcLine <$> (try userLine <|> prefix) 
              <*> command 
              <*> params
              <*> sToM `liftM` trail

nick :: Parser String
nick = char ':' >> manyTill anyChar (char '!')

user :: Parser String
user = manyTill anyChar (char '@')

host :: Parser String
host = manyTill anyChar space

userLine :: Parser IrcPrefix
userLine = User <$> nick <*> user <*> host

parseIrc :: String -> Either ParseError IrcLine
parseIrc = parse irc ""

sToM :: String -> Maybe String
sToM s = if null s then Nothing else Just s
