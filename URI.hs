{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative hiding (some)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Scheme = SchemeData
            | SchemeFile
            | SchemeFtp
            | SchemeHttp
            | SchemeHttps
            | SchemeIrc
            | SchemeMailto
            deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"
  , SchemeHttp   <$ string "http"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]

data Uri = Uri
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  , uriPath :: Maybe Path
  } deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text)
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)

data Path = Path
  { path :: Text
  } deriving (Eq, Show)

pAuthority :: Parser (Maybe Authority)
pAuthority = do
  uriAuthority <- optional . try $ do
    void (string "//")
    authUser <- optional . try $ do
      user <- T.pack <$> some alphaNumChar
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> L.decimal)
    return Authority {..}
  return uriAuthority

pPath :: Parser (Maybe Path)
pPath = do
  uriPath <- optional . try $ do
    void (char '/')
    path <- T.pack <$> some alphaNumChar
    return Path {..}
  return uriPath

pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme
  void (char ':')
  uriAuthority <- pAuthority
  uriPath <- pPath
  return Uri {..}

main = return ()
