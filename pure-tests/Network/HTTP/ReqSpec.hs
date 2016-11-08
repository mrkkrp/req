--
-- Tests for ‘req’ package. This test suite tests correctness of constructed
-- requests.
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
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#if __GLASGOW_HASKELL__ <  710
{-# LANGUAGE ConstraintKinds      #-}
#endif

module Network.HTTP.ReqSpec
  ( spec )
where

import Control.Exception (throwIO)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.Maybe (isNothing, fromJust)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Req
import Test.Hspec
import Test.Hspec.Core.Spec (SpecM)
import Test.QuickCheck
import qualified Blaze.ByteString.Builder as BB
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as B8
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Network.HTTP.Client      as L
import qualified Network.HTTP.Types       as Y

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid (mempty)
#endif

spec :: Spec
spec = do

  describe "config" $
    it "getHttpConfig has effect on resulting request" $
      property $ \config -> do
        request <- runReaderT (req_ GET url NoReqBody mempty) config
        L.proxy         request `shouldBe` httpConfigProxy         config
        L.redirectCount request `shouldBe` httpConfigRedirectCount config

  describe "methods" $ do
    let mnth
          :: forall method.
             ( HttpMethod method
             , HttpBodyAllowed (AllowsBody method) 'NoBody )
          => method
          -> SpecM () ()
        mnth method = do
          let name = httpMethodName (Proxy :: Proxy method)
          describe (B8.unpack name) $
            it "affects name of HTTP method" $ do
              request <- req_ method url NoReqBody mempty
              L.method request `shouldBe` name
    mnth GET
    mnth POST
    mnth HEAD
    mnth PUT
    mnth DELETE
    mnth TRACE
    mnth CONNECT
    mnth OPTIONS
    mnth PATCH

  describe "urls" $ do
    describe "http" $
      it "sets all the params correctly" $
        property $ \host -> do
          request <- req_ GET (http host) NoReqBody mempty
          L.secure request `shouldBe` False
          L.port   request `shouldBe` 80
          L.host   request `shouldBe` urlEncode host
    describe "https" $
      it "sets all the params correctly" $
        property $ \host -> do
          request <- req_ GET (https host) NoReqBody mempty
          L.secure request `shouldBe` True
          L.port   request `shouldBe` 443
          L.host   request `shouldBe` urlEncode host
    describe "(/~)" $
      it "attaches a path piece that is URL-encoded" $
        property $ \host pieces -> do
          let url' = foldl (/~) (https host) pieces
          request <- req_ GET url' NoReqBody mempty
          L.host request `shouldBe` urlEncode host
          L.path request `shouldBe` encodePathPieces pieces
    describe "(/:)" $
      it "attaches a path piece that is URL-encoded" $
        property $ \host pieces -> do
          let url' = foldl (/:) (https host) pieces
          request <- req_ GET url' NoReqBody mempty
          L.host request `shouldBe` urlEncode host
          L.path request `shouldBe` encodePathPieces pieces
    describe "parseUrlHttp" $ do
      it "does not recognize non-http schemes" $
        parseUrlHttp "https://httpbin.org" `shouldSatisfy` isNothing
      it "parses correct URLs" $
        property $ \host pieces queryParams ->
          not (T.null host) && wellFormed queryParams ==> do
          let (url', path, queryString) =
                assembleUrl Http host pieces queryParams
              (url'', options) = fromJust (parseUrlHttp url')
          request <- req_ GET url'' NoReqBody options
          L.host        request `shouldBe` urlEncode host
          L.path        request `shouldBe` path
          L.queryString request `shouldBe` queryString
    describe "parseUrlHttps" $ do
      it "does not recognize non-https schemes" $
        parseUrlHttps "http://httpbin.org" `shouldSatisfy` isNothing
      it "parses correct URLs" $
        property $ \host pieces queryParams ->
          not (T.null host) && wellFormed queryParams ==> do
          let (url', path, queryString) =
                assembleUrl Https host pieces queryParams
              (url'', options) = fromJust (parseUrlHttps url')
          request <- req_ GET url'' NoReqBody options
          L.host        request `shouldBe` urlEncode host
          L.path        request `shouldBe` path
          L.queryString request `shouldBe` queryString

  describe "bodies" $ do
    describe "NoReqBody" $ do
      it "sets body to empty byte string" $ do
        request <- req_ GET url NoReqBody mempty
        case L.requestBody request of
          L.RequestBodyBS x -> x `shouldBe` B.empty
          _ -> expectationFailure "Something is wrong with request body."
  -- TODO finish with request bodies

  -- TODO all options, OAuth1 and AWS pending

----------------------------------------------------------------------------
-- Instances

instance MonadHttp IO where
  handleHttpException = throwIO

instance MonadHttp (ReaderT HttpConfig IO) where
  handleHttpException = liftIO . throwIO
  getHttpConfig       = ask

instance Arbitrary HttpConfig where
  arbitrary = do
    httpConfigProxy         <- arbitrary
    httpConfigRedirectCount <- arbitrary
    let httpConfigAltManager = Nothing
        httpConfigCheckResponse _ _ = return ()
    return HttpConfig {..}

instance Show HttpConfig where
  show HttpConfig {..} =
    "HttpConfig\n" ++
    "{ httpConfigProxy=" ++ show httpConfigProxy ++ "\n" ++
    ", httpRedirectCount=" ++ show httpConfigRedirectCount ++ "\n" ++
    ", httpConfigAltManager=<cannot be shown>\n" ++
    ", httpConfigCheckResponse=<cannot be shown>}\n"

instance Arbitrary L.Proxy where
  arbitrary = L.Proxy <$> arbitrary <*> arbitrary

instance Arbitrary ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Show (Option scheme) where
  show _ = "<cannot be shown>"

----------------------------------------------------------------------------
-- Helpers

-- | 'req' that just returns the prepared 'L.Request'.

req_
  :: ( MonadHttp   m
     , HttpMethod  method
     , HttpBody    body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body) )
  => method            -- ^ HTTP method
  -> Url scheme        -- ^ 'Url' — location of resource
  -> body              -- ^ Body of the request
  -> Option scheme     -- ^ Collection of optional parameters
  -> m L.Request       -- ^ Vanilla request
req_ method url' body options =
  responseRequest `liftM` req method url' body returnRequest options

-- | A dummy 'Url'.

url :: Url 'Https
url = https "httpbin.org"

-- | Percent encode given 'Text'.

urlEncode :: Text -> ByteString
urlEncode = Y.urlEncode False . T.encodeUtf8

-- | Build URL path from given path pieces.

encodePathPieces :: [Text] -> ByteString
encodePathPieces = BL.toStrict . BB.toLazyByteString . Y.encodePathSegments

-- | Assemble entire URL.

assembleUrl
  :: Scheme            -- ^ Scheme
  -> Text              -- ^ Host
  -> [Text]            -- ^ Path pieces
  -> [(Text, Maybe Text)] -- ^ Query parameters
  -> (ByteString, ByteString, ByteString) -- ^ URL, path, query string
assembleUrl scheme' host' pathPieces queryParams =
  (scheme <> host <> path <> queryString, path, queryString)
  where
    scheme = case scheme' of
      Http  -> "http://"
      Https -> "https://"
    host        = urlEncode host'
    path        = encodePathPieces pathPieces
    queryString = Y.renderQuery True (Y.queryTextToQuery queryParams)

-- | Check if collection of query params is well-formed.

wellFormed :: [(Text, Maybe Text)] -> Bool
wellFormed = all (not . T.null . fst)
