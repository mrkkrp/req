{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS_GHC -fno-warn-orphans      #-}

module Network.HTTP.ReqSpec
  ( spec )
where

import Control.Exception (throwIO)
import Data.Aeson (ToJSON (..))
import Data.ByteString (ByteString)
import Data.Default.Class
import Data.Maybe (isNothing, fromJust)
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text (Text)
import Data.Time
import GHC.Generics
import Network.HTTP.Req
import Test.Hspec
import Test.Hspec.Core.Spec (SpecM)
import Test.QuickCheck
import qualified Blaze.ByteString.Builder as BB
import qualified Data.Aeson               as A
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as B8
import qualified Data.ByteString.Lazy     as BL
import qualified Data.CaseInsensitive     as CI
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Network.HTTP.Client      as L
import qualified Network.HTTP.Types       as Y

spec :: Spec
spec = do

  describe "config" $
    it "getHttpConfig has effect on resulting request" $
      property $ \config -> do
        request <- runReq config (req_ GET url NoReqBody mempty)
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
        property $ \host mport' pieces queryParams -> do
          let (url', path, queryString) =
                assembleUrl Http host mport' pieces queryParams
              (url'', options) = fromJust (parseUrlHttp url')
          request <- req_ GET url'' NoReqBody options
          L.host        request `shouldBe` urlEncode (unHost host)
          L.port        request `shouldBe` maybe 80 getNonNegative mport'
          L.path        request `shouldBe` path
          L.queryString request `shouldBe` queryString
      it "rejects gibberish in port component" $ do
        parseUrlHttp "http://my-site.com:bob/far" `shouldSatisfy` isNothing
        parseUrlHttp "http://my-site.com:7001uh/api" `shouldSatisfy` isNothing
        parseUrlHttp "http://my-site.com:/bar" `shouldSatisfy` isNothing
    describe "parseUrlHttps" $ do
      it "does not recognize non-https schemes" $
        parseUrlHttps "http://httpbin.org" `shouldSatisfy` isNothing
      it "parses correct URLs" $
        property $ \host mport' pieces queryParams -> do
          let (url', path, queryString) =
                assembleUrl Https host mport' pieces queryParams
              (url'', options) = fromJust (parseUrlHttps url')
          request <- req_ GET url'' NoReqBody options
          L.host        request `shouldBe` urlEncode (unHost host)
          L.port        request `shouldBe` maybe 443 getNonNegative mport'
          L.path        request `shouldBe` path
          L.queryString request `shouldBe` queryString
      it "rejects gibberish in port component" $ do
        parseUrlHttp "https://my-site.com:bob/far" `shouldSatisfy` isNothing
        parseUrlHttp "https://my-site.com:7001uh/api" `shouldSatisfy` isNothing
        parseUrlHttp "https://my-site.com:/bar" `shouldSatisfy` isNothing

  describe "bodies" $ do
    describe "NoReqBody" $
      it "sets body to empty byte string" $ do
        request <- req_ POST url NoReqBody mempty
        case L.requestBody request of
          L.RequestBodyBS x -> x `shouldBe` B.empty
          _ -> expectationFailure "Wrong request body constructor."
    describe "ReqBodyJson" $
      it "sets body to correct lazy byte string" $
        property $ \thing -> do
          request <- req_ POST url (ReqBodyJson thing) mempty
          case L.requestBody request of
            L.RequestBodyLBS x -> x `shouldBe` A.encode (thing :: Thing)
            _ -> expectationFailure "Wrong request body constructor."
    describe "ReqBodyBs" $
      it "sets body to specified strict byte string" $
        property $ \bs -> do
          request <- req_ POST url (ReqBodyBs bs) mempty
          case L.requestBody request of
            L.RequestBodyBS x -> x `shouldBe` bs
            _ -> expectationFailure "Wrong request body constructor."
    describe "ReqBodyLbs" $
     it "sets body to specified lazy byte string" $
        property $ \lbs -> do
          request <- req_ POST url (ReqBodyLbs lbs) mempty
          case L.requestBody request of
            L.RequestBodyLBS x -> x `shouldBe` lbs
            _ -> expectationFailure "Wrong request body constructor."
    describe "ReqBodyUrlEnc" $
      it "sets body to correct lazy byte string" $
        property $ \params -> do
          request <- req_ POST url (ReqBodyUrlEnc (formUrlEnc params)) mempty
          case L.requestBody request of
            L.RequestBodyLBS x -> x `shouldBe` renderQuery params
            _ -> expectationFailure "Wrong request body constructor."

  describe "optional parameters" $ do
    describe "header" $ do
      it "sets specified header value" $
        property $ \name value -> do
          request <- req_ GET url NoReqBody (header name value)
          lookup (CI.mk name) (L.requestHeaders request) `shouldBe` pure value
      it "left header wins" $
        property $ \name value0 value1 -> do
          request <- req_ GET url NoReqBody
            (header name value0 <> header name value1)
          lookup (CI.mk name) (L.requestHeaders request) `shouldBe` pure value0
      it "overwrites headers set by other parts of the lib" $
        property $ \value -> do
          request <- req_ POST url (ReqBodyUrlEnc mempty)
            (header "Content-Type" value)
          lookup "Content-Type" (L.requestHeaders request) `shouldBe` pure value
    describe "cookieJar" $
      it "cookie jar is set without modifications" $
        property $ \cjar -> do
          request <- req_ GET url NoReqBody (cookieJar cjar)
          L.cookieJar request `shouldBe` pure cjar
    describe "basicAuth" $ do
      it "sets Authorization header to correct value" $
        property $ \username password -> do
          request <- req_ GET url NoReqBody (basicAuth username password)
          lookup "Authorization" (L.requestHeaders request) `shouldBe`
            pure (basicAuthHeader username password)
      it "overwrites manual setting of header" $
        property $ \username password value -> do
          request0 <- req_ GET url NoReqBody
            (basicAuth username password <> header "Authorization" value)
          request1 <- req_ GET url NoReqBody
            (header "Authorization" value <> basicAuth username password)
          let result = basicAuthHeader username password
          lookup "Authorization" (L.requestHeaders request0) `shouldBe`
            pure result
          lookup "Authorization" (L.requestHeaders request1) `shouldBe`
            pure result
      it "left auth option wins" $
        property $ \username0 password0 username1 password1 -> do
          request <- req_ GET url NoReqBody
            (basicAuth username0 password0 <> basicAuth username1 password1)
          lookup "Authorization" (L.requestHeaders request) `shouldBe`
            pure (basicAuthHeader username0 password0)
    describe "oAuth2Bearer" $ do
      it "sets Authorization header to correct value" $
        property $ \token -> do
          request <- req_ GET url NoReqBody (oAuth2Bearer token)
          lookup "Authorization" (L.requestHeaders request) `shouldBe`
            pure ("Bearer " <> token)
      it "overwrites manual setting of header" $
        property $ \token value -> do
          request0 <- req_ GET url NoReqBody
            (oAuth2Bearer token <> header "Authorization" value)
          request1 <- req_ GET url NoReqBody
            (header "Authorization" value <> oAuth2Bearer token)
          let result = "Bearer " <> token
          lookup "Authorization" (L.requestHeaders request0) `shouldBe`
            pure result
          lookup "Authorization" (L.requestHeaders request1) `shouldBe`
            pure result
      it "left auth option wins" $
        property $ \token0 token1 -> do
          request <- req_ GET url NoReqBody
            (oAuth2Bearer token0 <> oAuth2Bearer token1)
          lookup "Authorization" (L.requestHeaders request) `shouldBe`
            pure ("Bearer " <> token0)
    describe "oAuth2Token" $ do
      it "sets Authorization header to correct value" $
        property $ \token -> do
          request <- req_ GET url NoReqBody (oAuth2Token token)
          lookup "Authorization" (L.requestHeaders request) `shouldBe`
            pure ("token " <> token)
      it "overwrites manual setting of header" $
        property $ \token value -> do
          request0 <- req_ GET url NoReqBody
            (oAuth2Token token <> header "Authorization" value)
          request1 <- req_ GET url NoReqBody
            (header "Authorization" value <> oAuth2Token token)
          let result = "token " <> token
          lookup "Authorization" (L.requestHeaders request0) `shouldBe`
            pure result
          lookup "Authorization" (L.requestHeaders request1) `shouldBe`
            pure result
      it "left auth option wins" $
        property $ \token0 token1 -> do
          request <- req_ GET url NoReqBody
            (oAuth2Token token0 <> oAuth2Token token1)
          lookup "Authorization" (L.requestHeaders request) `shouldBe`
            pure ("token " <> token0)
    describe "port" $
      it "sets port overwriting the defaults" $
        property $ \n -> do
          request <- req_ GET url NoReqBody (port n)
          L.port request `shouldBe` n
    describe "decompress" $
      it "sets decompress function overwriting the defaults" $
        property $ \token -> do
          request <- req_ GET url NoReqBody (decompress (/= token))
          L.decompress request token `shouldBe` False
    -- FIXME Can't really test responseTimeout right new because the
    -- ResponseTimeout data type does not implement Eq and its constructors
    -- are also not exported. Sent a PR.
    describe "httpVersion" $
      it "sets HTTP version overwriting the defaults" $
        property $ \major minor -> do
          request <- req_ GET url NoReqBody (httpVersion major minor)
          L.requestVersion request `shouldBe` Y.HttpVersion major minor

----------------------------------------------------------------------------
-- Instances

instance MonadHttp IO where
  handleHttpException = throwIO

instance Arbitrary HttpConfig where
  arbitrary = do
    httpConfigProxy         <- arbitrary
    httpConfigRedirectCount <- arbitrary
    let httpConfigAltManager          = Nothing
        httpConfigCheckResponse _ _ _ = Nothing
        httpConfigRetryPolicy         = def
        httpConfigRetryJudge      _ _ = False
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

instance Arbitrary BL.ByteString where
  arbitrary = BL.pack <$> arbitrary

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Show (Option scheme) where
  show _ = "<cannot be shown>"

data Thing = Thing
  { _thingBool :: Bool
  , _thingText :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Thing

instance Arbitrary Thing where
  arbitrary = Thing <$> arbitrary <*> arbitrary

instance Arbitrary L.CookieJar where
  arbitrary = L.createCookieJar <$> arbitrary

instance Arbitrary L.Cookie where
  arbitrary = do
    cookie_name          <- arbitrary
    cookie_value         <- arbitrary
    cookie_expiry_time   <- arbitrary
    cookie_domain        <- arbitrary
    cookie_path          <- arbitrary
    cookie_creation_time <- arbitrary
    cookie_last_access_time <- arbitrary
    cookie_persistent    <- arbitrary
    cookie_host_only     <- arbitrary
    cookie_secure_only   <- arbitrary
    cookie_http_only     <- arbitrary
    return L.Cookie {..}

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary DiffTime where
  arbitrary = secondsToDiffTime <$> arbitrary

----------------------------------------------------------------------------
-- Helper types

-- | A wrapper to generate correct hosts.

newtype Host = Host { unHost :: Text }
  deriving (Eq, Show)

instance Arbitrary Host where
  arbitrary = Host . T.pack <$> listOf1 (arbitrary `suchThat` (/= ':'))

-- | A wrapper to generate correct query parameters.

newtype QueryParams = QueryParams [(Text, Maybe Text)]
  deriving (Eq, Show)

instance Arbitrary QueryParams where
  arbitrary = QueryParams <$> (arbitrary `suchThat` wellFormed)
    where
      wellFormed = all (not . T.null . fst)

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
req_ method url' body options = req' method url' body options $
  \request _ -> return request

-- | A dummy 'Url'.

url :: Url 'Https
url = https "httpbin.org"

-- | Percent encode given 'Text'.

urlEncode :: Text -> ByteString
urlEncode = Y.urlEncode False . T.encodeUtf8

-- | Build URL path from given path pieces.

encodePathPieces :: [Text] -> ByteString
encodePathPieces = BL.toStrict . BB.toLazyByteString . Y.encodePathSegments

-- | Assemble a URL.

assembleUrl
  :: Scheme            -- ^ Scheme
  -> Host              -- ^ Host
  -> Maybe (NonNegative Int) -- ^ Port
  -> [Text]            -- ^ Path pieces
  -> QueryParams       -- ^ Query parameters
  -> (ByteString, ByteString, ByteString) -- ^ URL, path, query string
assembleUrl scheme' (Host host') mport' pathPieces (QueryParams queryParams) =
  (scheme <> host <> port' <> path <> queryString, path, queryString)
  where
    scheme = case scheme' of
      Http  -> "http://"
      Https -> "https://"
    host        = urlEncode host'
    port'       =
      case mport' of
        Nothing -> ""
        Just (NonNegative x) -> ":" <> B8.pack (show x)
    path        = encodePathPieces pathPieces
    queryString = Y.renderQuery True (Y.queryTextToQuery queryParams)

renderQuery :: [(Text, Maybe Text)] -> BL.ByteString
renderQuery = BL.fromStrict . Y.renderQuery False . Y.queryTextToQuery

-- | Convert collection of query parameters to 'FormUrlEncodedParam' thing.

formUrlEnc :: [(Text, Maybe Text)] -> FormUrlEncodedParam
formUrlEnc = foldMap (uncurry queryParam)

-- | Get “Authorization” basic auth header given username and password.

basicAuthHeader :: ByteString -> ByteString -> ByteString
basicAuthHeader username password =
  fromJust . lookup Y.hAuthorization . L.requestHeaders $
    L.applyBasicAuth username password L.defaultRequest
