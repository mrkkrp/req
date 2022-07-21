{-# LANGUAGE OverloadedStrings #-}

{-
 - Code in this module was copied, with modifications, from
 - Network.Wreq.Internal.Links in wreq. The original license for this code
 - follows:
 -
 - Copyright © 2014, Bryan O'Sullivan.
 -
 - All rights reserved.
 -
 - Redistribution and use in source and binary forms, with or without
 - modification, are permitted provided that the following conditions are met:
 -
 -     Redistributions of source code must retain the above copyright notice,
 -     this list of conditions and the following disclaimer.
 -
 -     Redistributions in binary form must reproduce the above copyright notice,
 -     this list of conditions and the following disclaimer in the documentation
 -     and/or other materials provided with the distribution.
 -
 -     Neither the name of nor the names of other contributors may be used to
 -     endorse or promote products derived from this software without specific
 -     prior written permission.
 -
 - THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 - AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 - IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 - ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 - LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 - CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 - SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 - INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 - CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 - ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 - POSSIBILITY OF SUCH DAMAGE.
-}

module Network.HTTP.Req.Links
  (
    Link(..),
    links,
  )
where

import Control.Applicative (many)
import Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString (ByteString)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Char8 as B8

-- | An element of a Link header.
data Link = Link {
      linkBs :: ByteString
    , linkParams :: [(ByteString, ByteString)]
    } deriving (Eq, Show)

links :: ByteString -> [Link]
links hdr = case parseOnly f hdr of
              Left _   -> []
              Right xs -> xs
  where f = sepBy1 (link <* skipSpace) (char8 ',' *> skipSpace) <* endOfInput

link :: Parser Link
link = Link <$> url <*> many (char8 ';' *> skipSpace *> param)
  where url = char8 '<' *> A8.takeTill (=='>') <* char8 '>' <* skipSpace

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
