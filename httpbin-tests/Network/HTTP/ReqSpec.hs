--
-- Tests for ‘req’ package. This test suite tests sending actual requests
-- using the <https://httpbin.org> service.
--
-- Copyright © 2016 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.ReqSpec
  ( spec )
where

import Control.Exception (throwIO)
import Control.Monad.Reader
import Data.Aeson (Value (..), object, (.=))
import Data.HashMap.Strict (HashMap)
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Req
import Test.Hspec
import qualified Data.HashMap.Strict as HM

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
#endif

spec :: Spec
spec = do

  describe "exception throwing on non 2xx-status codes" $ do
    it "doesn't throw for 200" pending
    it "throws indeed for non-404" pending

  describe "response check via httpConfigCheckResponse" $
    context "if it's set to always throw" $
      it "throws indeed" pending

  describe "receiving user-agent header back" $
    it "works" $ do
      r <- req GET (httpbin /: "user-agent")
        NoReqBody jsonResponse (header "user-agent" "Req")
      responseBody          r `shouldBe` object
        ["user-agent" .= ("Req" :: Text)]
      responseStatusCode    r `shouldBe` 200
      responseStatusMessage r `shouldBe` "OK"

  describe "receiving request headers back" $
    it "works" $ do
      r <- req GET (httpbin /: "headers")
        NoReqBody jsonResponse (header "Foo" "bar" <> header "Baz" "quux")
      responseBody          r `shouldBe` object
        [ "headers" .= object
          [ "Accept-Encoding" .= ("gzip"        :: Text)
          , "Foo"             .= ("bar"         :: Text)
          , "Baz"             .= ("quux"        :: Text)
          , "Host"            .= ("httpbin.org" :: Text) ] ]
      responseStatusCode    r `shouldBe` 200
      responseStatusMessage r `shouldBe` "OK"

  describe "receiving GET data back" $
    it "works" $ do
      r <- req GET (httpbin /: "get") NoReqBody jsonResponse mempty
      stripOrigin (responseBody r) `shouldBe` object
        [ "args" .= (HM.empty :: HashMap Text Text)
        , "url"  .= ("https://httpbin.org/get" :: Text)
        , "headers" .= object
          [ "Accept-Encoding" .= ("gzip"        :: Text)
          , "Host"            .= ("httpbin.org" :: Text) ] ]
      responseStatusCode      r `shouldBe` 200
      responseStatusMessage   r `shouldBe` "OK"

----------------------------------------------------------------------------
-- Instances

instance MonadHttp IO where
  handleHttpException = throwIO

instance MonadHttp (ReaderT HttpConfig IO) where
  handleHttpException = liftIO . throwIO
  getHttpConfig       = ask

----------------------------------------------------------------------------
-- Helpers

-- | 'Url' representing <https://httpbin.org>.

httpbin :: Url 'Https
httpbin = https "httpbin.org"

-- | Remove “origin” field from JSON value. Origin may change, we don't want
-- to depend on that.

stripOrigin :: Value -> Value
stripOrigin (Object m) = Object (HM.delete "origin" m)
stripOrigin value      = value
