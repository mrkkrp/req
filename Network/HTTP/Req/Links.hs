{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Req.Links
  (
    Link(..),
    links,
  )
where

import Text.Megaparsec
import qualified Text.Megaparsec.Byte as Byte
import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Char8 as B8

type Parser = Parsec () ByteString

-- | An element of a Link header.
data Link = Link {
      linkBs :: ByteString
    , linkParams :: [(ByteString, ByteString)]
    } deriving (Eq, Show)

links :: ByteString -> [Link]
links hdr = case parse f "" hdr of
              Left _   -> []
              Right xs -> xs
  where f = sepBy1 (link <* Byte.space) (Byte.char ',' *> Byte.space) <* Byte.eol

link :: Parser Link
link = Link <$> url <*> many (Byte.char ';' *> Byte.space *> param)
  where url = Byte.char '<' *> takeWhileP (Just "url char") (/='>') <* Byte.char '>' <* Byte.space

param :: Parser (ByteString, ByteString)
param = do
  name <- paramName
  skipSpace *> "=" *> skipSpace
  c <- peekChar'
  let isTokenChar = A.inClass "!#$%&'()*+./0-9:<=>?@a-zA-Z[]^_`{|}~-"
  val <- case c of
           '"' -> quotedString
           _   -> A.takeWhile isTokenChar
  skipSpace
  return (name, val)

data Quot = Literal | Backslash

quotedString :: Parser ByteString
quotedString = char '"' *> (fixup <$> body) <* char '"'
  where body = A8.scan Literal $ \s c ->
          case (s,c) of
            (Literal,  '\\') -> backslash
            (Literal,  '"')  -> Nothing
            _                -> literal
        literal   = Just Literal
        backslash = Just Backslash
        fixup = B8.pack . go . B8.unpack
          where go ('\\' : x@'\\' : xs) = x : go xs
                go ('\\' : x@'"' : xs)  = x : go xs
                go (x : xs)             = x : go xs
                go []                   = []

paramName :: Parser ByteString
paramName = A.takeWhile1 $ A.inClass "a-zA-Z0-9!#$&+-.^_`|~"
