{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.ReqSpec
  ( spec )
where

import Control.Exception (throwIO)
import Control.Monad.Reader
import Data.Aeson (Value (..), ToJSON (..), object, (.=))
import Data.Default.Class
import Data.Monoid ((<>))
import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Req
import Test.Hspec
import Test.QuickCheck
import qualified Data.Aeson           as A
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as TIO
import qualified Network.HTTP.Client  as L
import qualified Network.HTTP.Client.MultipartFormData as LM
import qualified Network.HTTP.Types   as Y

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
import Data.Word (Word)
#endif

spec :: Spec
spec = do

  describe "exception throwing on non 2xx-status codes" $
    it "throws indeed for non-2xx" $ do
      let selector :: HttpException -> Bool
          selector (VanillaHttpException
                    (L.HttpExceptionRequest _
                     (L.StatusCodeException response chunk))) =
            L.responseStatus response == Y.status404 && not (B.null chunk)
          selector _ = False
      req GET (httpbin /: "foo") NoReqBody ignoreResponse mempty
        `shouldThrow` selector

  describe "response check via httpConfigCheckResponse" $
    context "if it's set to always throw" $
      it "throws indeed" $
        blindlyThrowing (req GET httpbin NoReqBody ignoreResponse mempty)
          `shouldThrow` anyException

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
      stripFunnyHeaders (responseBody r) `shouldBe` object
        [ "headers" .= object
          [ "Accept-Encoding" .= ("gzip"        :: Text)
          , "Connection"      .= ("close"       :: Text)
          , "Foo"             .= ("bar"         :: Text)
          , "Baz"             .= ("quux"        :: Text)
          , "Host"            .= ("httpbin.org" :: Text) ] ]
      responseStatusCode    r `shouldBe` 200
      responseStatusMessage r `shouldBe` "OK"

  describe "receiving GET data back" $
    it "works" $ do
      r <- req GET (httpbin /: "get") NoReqBody jsonResponse mempty
      (stripFunnyHeaders . stripOrigin) (responseBody r) `shouldBe` object
        [ "args" .= emptyObject
        , "url"  .= ("https://httpbin.org/get" :: Text)
        , "headers" .= object
          [ "Accept-Encoding" .= ("gzip"        :: Text)
          , "Connection"      .= ("close"       :: Text)
          , "Host"            .= ("httpbin.org" :: Text) ] ]
      responseHeader r "Content-Type" `shouldBe` return "application/json"
      responseStatusCode    r `shouldBe` 200
      responseStatusMessage r `shouldBe` "OK"

  describe "receiving POST JSON data back" $
    it "works" $ do
      let text = "foo" :: Text
          reflected = reflectJSON text
      r <- req POST (httpbin /: "post") (ReqBodyJson text) jsonResponse mempty
      (stripFunnyHeaders . stripOrigin) (responseBody r) `shouldBe` object
        [ "args"  .= emptyObject
        , "json"  .= text
        , "data"  .= reflected
        , "url"   .= ("https://httpbin.org/post" :: Text)
        , "headers" .= object
          [ "Content-Type"   .= ("application/json; charset=utf-8" :: Text)
          , "Accept-Encoding" .= ("gzip"       :: Text)
          , "Connection"      .= ("close"      :: Text)
          , "Host"           .= ("httpbin.org" :: Text)
          , "Content-Length" .= show (T.length reflected) ]
        , "files" .= emptyObject
        , "form"  .= emptyObject ]
      responseHeader r "Content-Type" `shouldBe` return "application/json"
      responseStatusCode    r `shouldBe` 200
      responseStatusMessage r `shouldBe` "OK"

  describe "receiving POST data back (multipart form data)" $
    it "works" $ do
      body <- reqBodyMultipart
        [ LM.partBS "foo" "foo data!"
        , LM.partBS "bar" "bar data!" ]
      r <- req POST (httpbin /: "post") body jsonResponse mempty
      let Just contentType = getRequestContentType body
      (stripFunnyHeaders . stripOrigin) (responseBody r) `shouldBe` object
        [ "args"  .= emptyObject
        , "json"  .= Null
        , "data"  .= ("" :: Text)
        , "url"   .= ("https://httpbin.org/post" :: Text)
        , "headers" .= object
          [ "Content-Type"    .= T.decodeUtf8 contentType
          , "Accept-Encoding" .= ("gzip"       :: Text)
          , "Connection"      .= ("close"      :: Text)
          , "Host"            .= ("httpbin.org" :: Text)
          , "Content-Length"  .= ("242" :: Text)
          ]
        , "files" .= emptyObject
        , "form"  .= object
          [ "foo" .= ("foo data!" :: Text)
          , "bar" .= ("bar data!" :: Text) ]
        ]
      responseHeader r "Content-Type" `shouldBe` return "application/json"
      responseStatusCode    r `shouldBe` 200
      responseStatusMessage r `shouldBe` "OK"

  describe "receiving PATCHed file back" $
    it "works" $ do
      let file :: FilePath
          file = "httpbin-data/robots.txt"
      contents <- TIO.readFile file
      r <- req PATCH (httpbin /: "patch") (ReqBodyFile file) jsonResponse mempty
      (stripFunnyHeaders . stripOrigin) (responseBody r) `shouldBe` object
        [ "args"  .= emptyObject
        , "json"  .= Null
        , "data"  .= contents
        , "url"   .= ("https://httpbin.org/patch" :: Text)
        , "headers" .= object
          [ "Accept-Encoding" .= ("gzip"       :: Text)
          , "Connection"      .= ("close"      :: Text)
          , "Host"           .= ("httpbin.org" :: Text)
          , "Content-Length" .= show (T.length contents) ]
        , "files" .= emptyObject
        , "form"  .= emptyObject ]
      responseHeader r "Content-Type" `shouldBe` return "application/json"
      responseStatusCode    r `shouldBe` 200
      responseStatusMessage r `shouldBe` "OK"

  describe "receiving PUT form URL-encoded data back" $
    it "works" $ do
      let params = "foo" =: ("bar" :: Text) <>
            "baz" =: (5 :: Int) <>
            queryFlag "quux"
      r <- req PUT (httpbin /: "put") (ReqBodyUrlEnc params) jsonResponse mempty
      (stripFunnyHeaders . stripOrigin) (responseBody r) `shouldBe` object
        [ "args"  .= emptyObject
        , "json"  .= Null
        , "data"  .= ("" :: Text)
        , "url"   .= ("https://httpbin.org/put" :: Text)
        , "headers" .= object
          [ "Content-Type"   .= ("application/x-www-form-urlencoded" :: Text)
          , "Accept-Encoding" .= ("gzip"       :: Text)
          , "Connection"      .= ("close"      :: Text)
          , "Host"           .= ("httpbin.org" :: Text)
          , "Content-Length" .= ("18"          :: Text) ]
        , "files" .= emptyObject
        , "form"  .= object
          [ "foo" .= ("bar" :: Text)
          , "baz" .= ("5"   :: Text)
          , "quux" .= (""   :: Text) ] ]
      responseHeader r "Content-Type" `shouldBe` return "application/json"
      responseStatusCode    r `shouldBe` 200
      responseStatusMessage r `shouldBe` "OK"

  -- TODO /delete

  describe "receiving UTF-8 encoded Unicode data" $
    it "works" $ do
      r <- req GET (httpbin /: "encoding" /: "utf8")
        NoReqBody bsResponse mempty
      utf8data <- B.readFile "httpbin-data/utf8.html"
      responseBody          r `shouldBe` utf8data
      responseStatusCode    r `shouldBe` 200
      responseStatusMessage r `shouldBe` "OK"

  -- TODO /gzip
  -- TODO /deflate

  forM_ [101..102] checkStatusCode
  forM_ [200..208] checkStatusCode
  -- forM_ [300..308] checkStatusCode
  forM_ [400..431] checkStatusCode
  forM_ [500..511] checkStatusCode

  -- TODO /response-headers
  -- TODO /redirect

  describe "redirects" $
    it "follows redirects" $ do
      r <- req GET (httpbin /: "redirect-to") NoReqBody ignoreResponse
        ("url" =: ("https://httpbin.org" :: Text))
      responseStatusCode    r `shouldBe` 200
      responseStatusMessage r `shouldBe` "OK"

  -- TODO /relative-redicet
  -- TODO /absolute-redirect
  -- TODO /cookies

  describe "basicAuth" $ do
    let user, password :: Text
        user     = "Scooby"
        password = "Doo"
    context "when we do not send appropriate basic auth data" $
      it "fails with 401" $ do
        r <- prepareForShit $ req GET
          (httpbin /: "basic-auth" /~ user /~ password)
          NoReqBody ignoreResponse mempty
        responseStatusCode    r `shouldBe` 401
        responseStatusMessage r `shouldBe` "UNAUTHORIZED"
    context "when we provide appropriate basic auth data" $
      it "succeeds" $ do
        r <- req GET (httpbin /: "basic-auth" /~ user /~ password)
          NoReqBody ignoreResponse
          (basicAuth (T.encodeUtf8 user) (T.encodeUtf8 password))
        responseStatusCode    r `shouldBe` 200
        responseStatusMessage r `shouldBe` "OK"

  -- TODO /hidden-basic-auth
  -- TODO /digest-auth
  -- TODO /stream
  -- TODO /delay
  -- TODO /drip
  -- TODO /range
  -- TODO /html

  describe "robots.txt" $
    it "works" $ do
      r <- req GET (httpbin /: "robots.txt") NoReqBody bsResponse mempty
      robots <- B.readFile "httpbin-data/robots.txt"
      responseBody          r `shouldBe` robots
      responseStatusCode    r `shouldBe` 200
      responseStatusMessage r `shouldBe` "OK"

  -- TODO /deny
  -- TODO /cache

  describe "getting random bytes" $ do
    it "works" $
      property $ \n' -> do
        let n :: Word
            n = getSmall n'
        r <- req GET (httpbin /: "bytes" /~ n)
          NoReqBody lbsResponse mempty
        responseBody r `shouldSatisfy` ((== n) . fromIntegral . BL.length)
        responseStatusCode    r `shouldBe` 200
        responseStatusMessage r `shouldBe` "OK"
    context "when we try to interpret 1000 random bytes as JSON" $
      it "throws correct exception" $ do
        let selector :: HttpException -> Bool
            selector (JsonHttpException _) = True
            selector _ = False
            n :: Int
            n = 1000
        req GET (httpbin /: "bytes" /~ n) NoReqBody
          (Proxy :: Proxy (JsonResponse Value)) mempty
            `shouldThrow` selector

  describe "streaming random bytes" $
    it "works" $
      property $ \n' -> do
        let n :: Word
            n = getSmall n'
        r <- req GET (httpbin /: "stream-bytes" /~ n)
          NoReqBody bsResponse mempty
        responseBody r `shouldSatisfy` ((== n) . fromIntegral . B.length)
        responseStatusCode    r `shouldBe` 200
        responseStatusMessage r `shouldBe` "OK"

  -- TODO /links
  -- TODO /image
  -- TODO /image/png
  -- TODO /image/jpeg
  -- TODO /image/webp
  -- TODO /image/svg
  -- TODO /forms/post
  -- TODO /xml

----------------------------------------------------------------------------
-- Instances

instance MonadHttp IO where
  handleHttpException = throwIO

instance MonadHttp (ReaderT HttpConfig IO) where
  handleHttpException = liftIO . throwIO
  getHttpConfig       = ask

----------------------------------------------------------------------------
-- Helpers

-- | Run request with such settings that it does not signal error on adverse
-- response status codes.

prepareForShit
  :: (forall m. MonadHttp m => m a)
  -> IO a
prepareForShit m = runReaderT m def { httpConfigCheckResponse = noNoise }
  where noNoise _ _ = return ()

-- | Run request with such settings that it throws on any response.

blindlyThrowing
  :: ReaderT HttpConfig IO a
  -> IO a
blindlyThrowing m = runReaderT m def { httpConfigCheckResponse = doit }
  where doit _ _ = error "Oops!"

-- | 'Url' representing <https://httpbin.org>.

httpbin :: Url 'Https
httpbin = https "httpbin.org"

-- | Remove “origin” field from JSON value. Origin may change, we don't want
-- to depend on that.

stripOrigin :: Value -> Value
stripOrigin (Object m) = Object (HM.delete "origin" m)
stripOrigin value      = value

-- | Remove funny headers that might break the tests.

stripFunnyHeaders :: Value -> Value
stripFunnyHeaders (Object m) =
  let f (Object p) = Object $ HM.filterWithKey (\k _ -> k `elem` hs) p
      f value      = value
      hs = [ "Content-Type"
           , "Accept-Encoding"
           , "Connection"
           , "Host"
           , "Content-Length"
           , "Foo"
           , "Baz" ]
  in Object (HM.adjust f "headers" m)
stripFunnyHeaders value = value

-- | This is a complete test case that makes use of <https://httpbin.org> to
-- get various response status codes.

checkStatusCode :: Int -> SpecWith ()
checkStatusCode code =
  describe ("receiving status code " ++ show code) $
    it "works" $ do
      r <- prepareForShit $ req GET (httpbin /: "status" /~ code)
        NoReqBody ignoreResponse mempty
      responseStatusCode r `shouldBe` code

-- | Empty JSON 'Object'.

emptyObject :: Value
emptyObject = Object HM.empty

-- | Get rendered JSON value as 'Text'.

reflectJSON :: ToJSON a => a -> Text
reflectJSON = T.decodeUtf8 . BL.toStrict . A.encode
