{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.ReqSpec (spec) where

import qualified Blaze.ByteString.Builder as BB
import Control.Exception (throwIO)
import Control.Monad
import Control.Retry
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.CaseInsensitive as CI
import Data.Either (isRight)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Typeable (Typeable, eqT)
import GHC.Generics
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Network.HTTP.Client as L
import Network.HTTP.Req
import qualified Network.HTTP.Types as Y
import qualified Network.HTTP.Types.Header as Y
import Test.Hspec
import Test.Hspec.Core.Spec (SpecM)
import Test.QuickCheck
import Text.URI (URI)
import qualified Text.URI as URI
import qualified Text.URI.QQ as QQ

spec :: Spec
spec = do
  describe "config" $
    it "getHttpConfig has effect on resulting request" $
      property $ \config -> do
        request <- runReq config (req_ GET url NoReqBody mempty)
        L.proxy request `shouldBe` httpConfigProxy config
        L.redirectCount request `shouldBe` httpConfigRedirectCount config

  describe "methods" $ do
    let mnth ::
          forall method.
          ( HttpMethod method,
            HttpBodyAllowed (AllowsBody method) 'NoBody
          ) =>
          method ->
          SpecM () ()
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
          L.port request `shouldBe` 80
          L.host request `shouldBe` urlEncode host
    describe "https" $
      it "sets all the params correctly" $
        property $ \host -> do
          request <- req_ GET (https host) NoReqBody mempty
          L.secure request `shouldBe` True
          L.port request `shouldBe` 443
          L.host request `shouldBe` urlEncode host
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

    describe "useHttpURI" $ do
      it "does not recognize non-http schemes" $
        property $ \uri ->
          when (URI.uriScheme uri /= Just [QQ.scheme|http|]) $
            useHttpURI uri `shouldSatisfy` isNothing
      it "accepts correct URLs" $
        property $ \uri' -> do
          unless (isRight (URI.uriAuthority uri')) discard
          let uri = uri' {URI.uriScheme = Just [QQ.scheme|http|]}
              (url', options) = fromJust (useHttpURI uri)
          request <- req_ GET url' NoReqBody options
          L.host request `shouldBe` uriHost uri
          L.port request `shouldBe` uriPort 80 uri
          L.path request `shouldBe` uriPath uri
          L.queryString request `shouldBe` uriQuery uri
          lookup "Authorization" (L.requestHeaders request)
            `shouldBe` uriBasicAuth uri
    describe "useHttpsURI" $ do
      it "does not recognize non-https schemes" $
        property $ \uri ->
          when (URI.uriScheme uri /= Just [QQ.scheme|https|]) $
            useHttpsURI uri `shouldSatisfy` isNothing
      it "parses correct URLs" $
        property $ \uri' -> do
          unless (isRight (URI.uriAuthority uri')) discard
          let uri = uri' {URI.uriScheme = Just [QQ.scheme|https|]}
              (url', options) = fromJust (useHttpsURI uri)
          request <- req_ GET url' NoReqBody options
          L.host request `shouldBe` uriHost uri
          L.port request `shouldBe` uriPort 443 uri
          L.path request `shouldBe` uriPath uri
          L.queryString request `shouldBe` uriQuery uri
          lookup "Authorization" (L.requestHeaders request)
            `shouldBe` uriBasicAuth uri
    describe "useURI" $ do
      it "does not recognize non-http and non-https schemes" $
        property $ \uri ->
          when
            ( ( URI.uriScheme uri /= Just [QQ.scheme|http|]
                  && (URI.uriScheme uri /= Just [QQ.scheme|https|])
              )
            )
            $ useURI uri `shouldSatisfy` isNothing
      it "parses correct URLs" $
        property $ \uri' -> do
          unless (isRight (URI.uriAuthority uri')) discard
          let uriHttp = uri' {URI.uriScheme = Just [QQ.scheme|http|]}
              uriHttps = uri' {URI.uriScheme = Just [QQ.scheme|https|]}
          requestHttp <-
            let Left (url', options) = fromJust (useURI uriHttp)
             in req_ GET url' NoReqBody options
          requestHttps <-
            let Right (url', options) = fromJust (useURI uriHttps)
             in req_ GET url' NoReqBody options
          L.host requestHttp `shouldBe` uriHost uriHttp
          L.host requestHttps `shouldBe` uriHost uriHttps
          L.port requestHttp `shouldBe` uriPort 80 uriHttp
          L.port requestHttps `shouldBe` uriPort 443 uriHttps
          L.path requestHttp `shouldBe` uriPath uriHttp
          L.path requestHttps `shouldBe` uriPath uriHttps
          L.queryString requestHttp `shouldBe` uriQuery uriHttp
          L.queryString requestHttps `shouldBe` uriQuery uriHttps
          lookup "Authorization" (L.requestHeaders requestHttp)
            `shouldBe` uriBasicAuth uriHttp
          lookup "Authorization" (L.requestHeaders requestHttps)
            `shouldBe` uriBasicAuth uriHttps

    describe "renderUrl" $ do
      context "http" $ do
        context "empty path" $
          it "renders correctly" $ do
            let (uriHttp, _) = [urlQ|http://httpbin.org|]
            renderUrl uriHttp `shouldBe` "http://httpbin.org"
        context "non-empty path" $
          it "renders correctly" $ do
            let (uriHttp, _) = [urlQ|http://httpbin.org/here/we/go|]
            renderUrl uriHttp `shouldBe` "http://httpbin.org/here/we/go"
      context "http" $ do
        context "empty path" $
          it "renders correctly" $ do
            let (uriHttp, _) = [urlQ|https://httpbin.org|]
            renderUrl uriHttp `shouldBe` "https://httpbin.org"
        context "non-empty path" $
          it "renders correctly" $ do
            let (uriHttp, _) = [urlQ|https://httpbin.org/here/we/go|]
            renderUrl uriHttp `shouldBe` "https://httpbin.org/here/we/go"

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
          request <-
            req_
              GET
              url
              NoReqBody
              (header name value0 <> header name value1)
          lookup (CI.mk name) (L.requestHeaders request) `shouldBe` pure value0
      it "overwrites headers set by other parts of the lib" $
        property $ \value -> do
          request <-
            req_
              POST
              url
              (ReqBodyUrlEnc mempty)
              (header "Content-Type" value)
          lookup "Content-Type" (L.requestHeaders request) `shouldBe` pure value
    describe "cookieJar" $
      it "cookie jar is set without modifications" $
        property $ \cjar -> do
          request <- req_ GET url NoReqBody (cookieJar cjar)
          L.cookieJar request `shouldSatisfy` (maybe False (L.equalCookieJar cjar))
    describe "basicAuth" $ do
      it "sets Authorization header to correct value" $
        property $ \username password -> do
          request <- req_ GET url NoReqBody (basicAuth username password)
          lookup "Authorization" (L.requestHeaders request)
            `shouldBe` Just (basicAuthHeader username password)
      it "overwrites manual setting of header" $
        property $ \username password value -> do
          request0 <-
            req_
              GET
              url
              NoReqBody
              (basicAuth username password <> header "Authorization" value)
          request1 <-
            req_
              GET
              url
              NoReqBody
              (header "Authorization" value <> basicAuth username password)
          let result = Just (basicAuthHeader username password)
          lookup "Authorization" (L.requestHeaders request0) `shouldBe` result
          lookup "Authorization" (L.requestHeaders request1) `shouldBe` result
      it "left auth option wins" $
        property $ \username0 password0 username1 password1 -> do
          request <-
            req_
              GET
              url
              NoReqBody
              (basicAuth username0 password0 <> basicAuth username1 password1)
          lookup "Authorization" (L.requestHeaders request)
            `shouldBe` Just (basicAuthHeader username0 password0)
    describe "oAuth2Bearer" $ do
      it "sets Authorization header to correct value" $
        property $ \token -> do
          request <- req_ GET url NoReqBody (oAuth2Bearer token)
          lookup "Authorization" (L.requestHeaders request)
            `shouldBe` pure ("Bearer " <> token)
      it "overwrites manual setting of header" $
        property $ \token value -> do
          request0 <-
            req_
              GET
              url
              NoReqBody
              (oAuth2Bearer token <> header "Authorization" value)
          request1 <-
            req_
              GET
              url
              NoReqBody
              (header "Authorization" value <> oAuth2Bearer token)
          let result = "Bearer " <> token
          lookup "Authorization" (L.requestHeaders request0)
            `shouldBe` pure result
          lookup "Authorization" (L.requestHeaders request1)
            `shouldBe` pure result
      it "left auth option wins" $
        property $ \token0 token1 -> do
          request <-
            req_
              GET
              url
              NoReqBody
              (oAuth2Bearer token0 <> oAuth2Bearer token1)
          lookup "Authorization" (L.requestHeaders request)
            `shouldBe` pure ("Bearer " <> token0)
    describe "ProxyAuthorization" $
      it "sets Authorization header to correct value" $
        property $ \username password -> do
          request <- req_ GET url NoReqBody (basicProxyAuth username password)
          lookup "Proxy-Authorization" (L.requestHeaders request)
            `shouldBe` pure (basicProxyAuthHeader username password)
    describe "oAuth2Token" $ do
      it "sets Authorization header to correct value" $
        property $ \token -> do
          request <- req_ GET url NoReqBody (oAuth2Token token)
          lookup "Authorization" (L.requestHeaders request)
            `shouldBe` pure ("token " <> token)
      it "overwrites manual setting of header" $
        property $ \token value -> do
          request0 <-
            req_
              GET
              url
              NoReqBody
              (oAuth2Token token <> header "Authorization" value)
          request1 <-
            req_
              GET
              url
              NoReqBody
              (header "Authorization" value <> oAuth2Token token)
          let result = "token " <> token
          lookup "Authorization" (L.requestHeaders request0)
            `shouldBe` pure result
          lookup "Authorization" (L.requestHeaders request1)
            `shouldBe` pure result
      it "left auth option wins" $
        property $ \token0 token1 -> do
          request <-
            req_
              GET
              url
              NoReqBody
              (oAuth2Token token0 <> oAuth2Token token1)
          lookup "Authorization" (L.requestHeaders request)
            `shouldBe` pure ("token " <> token0)
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

    describe "quasiquoter" $ do
      it "works for valid urls" $
        -- Doing it this way instead of just checking if [urlQ|...|] :: (Url
        -- 'Https, Option _) type checks, so we can catch if the type of scheme
        -- is unspecified.
        let testTypeOfQuoterResult ::
              forall a s. Typeable a => (a, Option s) -> Bool
            testTypeOfQuoterResult _ = isJust $ eqT @a @(Url 'Https)
         in property $ testTypeOfQuoterResult [urlQ|https://example.org/|]
      it "doesn't work for invalid urls" $
        property $
          TH.runQ (TH.quoteExp urlQ "not a url") `shouldThrow` anyIOException

----------------------------------------------------------------------------
-- Instances

instance MonadHttp IO where
  handleHttpException = throwIO

instance Arbitrary HttpConfig where
  arbitrary = do
    httpConfigProxy <- arbitrary
    httpConfigRedirectCount <- arbitrary
    let httpConfigAltManager = Nothing
        httpConfigCheckResponse _ _ _ = Nothing
        httpConfigRetryPolicy = retryPolicyDefault
        httpConfigRetryJudge _ _ = False
        httpConfigRetryJudgeException _ _ = False
        httpConfigBodyPreviewLength = 1024
    return HttpConfig {..}

instance Show HttpConfig where
  show HttpConfig {..} =
    "HttpConfig\n"
      ++ "{ httpConfigProxy="
      ++ show httpConfigProxy
      ++ "\n"
      ++ ", httpRedirectCount="
      ++ show httpConfigRedirectCount
      ++ "\n"
      ++ ", httpConfigAltManager=<cannot be shown>\n"
      ++ ", httpConfigCheckResponse=<cannot be shown>}\n"

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
  { _thingBool :: Bool,
    _thingText :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Thing

instance Arbitrary Thing where
  arbitrary = Thing <$> arbitrary <*> arbitrary

instance Arbitrary L.CookieJar where
  arbitrary = L.createCookieJar <$> arbitrary

instance Arbitrary L.Cookie where
  arbitrary = do
    cookie_name <- arbitrary
    cookie_value <- arbitrary
    cookie_expiry_time <- arbitrary
    cookie_domain <- arbitrary
    cookie_path <- arbitrary
    cookie_creation_time <- arbitrary
    cookie_last_access_time <- arbitrary
    cookie_persistent <- arbitrary
    cookie_host_only <- arbitrary
    cookie_secure_only <- arbitrary
    cookie_http_only <- arbitrary
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
newtype Host = Host {unHost :: Text}
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
req_ ::
  ( MonadHttp m,
    HttpMethod method,
    HttpBody body,
    HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
  ) =>
  -- | HTTP method
  method ->
  -- | 'Url' — location of resource
  Url scheme ->
  -- | Body of the request
  body ->
  -- | Collection of optional parameters
  Option scheme ->
  -- | Vanilla request
  m L.Request
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

-- | Get host from a 'URI'. This function is not total.
uriHost :: URI -> ByteString
uriHost uri =
  fromJust $
    urlEncode . URI.unRText . URI.authHost
      <$> either (const Nothing) Just (URI.uriAuthority uri)

-- | Get part from a 'URI' defaulting to the provided value.
uriPort :: Int -> URI -> Int
uriPort def uri =
  maybe def (fromIntegral) $
    either (const Nothing) Just (URI.uriAuthority uri) >>= URI.authPort

-- | Get the path from a 'URI'.
uriPath :: URI -> ByteString
uriPath uri = fromMaybe "" $ do
  (trailingSlash, xs) <- URI.uriPath uri
  let pref = (encodePathPieces . fmap URI.unRText . NE.toList) xs
  return $ if trailingSlash then pref <> "/" else pref

-- | Get the query string from a 'URI'.
uriQuery :: URI -> ByteString
uriQuery uri = do
  let liftQueryParam = \case
        URI.QueryFlag t -> (URI.unRText t, Nothing)
        URI.QueryParam k v -> (URI.unRText k, Just (URI.unRText v))
  Y.renderQuery True (Y.queryTextToQuery (liftQueryParam <$> (URI.uriQuery uri)))

-- | Predict the headrs that should be set if the given 'URI' has username
-- and password in it.
uriBasicAuth :: URI -> Maybe ByteString
uriBasicAuth uri = do
  auth <- either (const Nothing) Just (URI.uriAuthority uri)
  URI.UserInfo {..} <- URI.authUserInfo auth
  let username = T.encodeUtf8 (URI.unRText uiUsername)
      password = maybe "" (T.encodeUtf8 . URI.unRText) uiPassword
  return (basicAuthHeader username password)

-- | Render a query as lazy 'BL.ByteString'.
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

-- | Get "Proxy-Authorization" basic proxy auth header given username and
-- password.
basicProxyAuthHeader :: ByteString -> ByteString -> ByteString
basicProxyAuthHeader username password =
  fromJust . lookup Y.hProxyAuthorization . L.requestHeaders $
    L.applyBasicProxyAuth username password L.defaultRequest
