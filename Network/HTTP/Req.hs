-- |
-- Module      :  Network.HTTP.Req
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The documentation below is structured in such a way that most important
-- information goes first: you learn how to do HTTP requests, then how to
-- embed them in any monad you have, then it goes on giving you details
-- about less-common things you may want to know about. The documentation is
-- written with sufficient coverage of details and examples, it's designed
-- to be a complete tutorial on its own.
--
-- /(A modest intro goes here, click on 'req' to start making requests.)/
--
-- === About the library
--
-- This is an easy-to-use, type-safe, expandable, high-level HTTP library
-- that just works without any fooling around.
--
-- What does the “easy-to-use” phrase mean? It means that the library is
-- designed to be beginner-friendly, so it's simple to add it to your monad
-- stack, intuitive to work with, well-documented, and does not get in your
-- way. Doing HTTP requests is a common task and Haskell library for this
-- should be very approachable and clear to beginners, thus certain
-- compromises were made. For example, one cannot currently modify
-- 'L.ManagerSettings' of default manager because the library always use the
-- same implicit global manager for simplicity and maximal connection
-- sharing. There is a way to use your own manager with different settings,
-- but it requires a bit more typing.
--
-- “Type-safe” means that the library is protective and eliminates certain
-- class of errors compared to alternative libraries like @wreq@ or vanilla
-- @http-client@ and friends. For example, we have correct-by-construction
-- 'Url's, it's guaranteed that user does not send request body when using
-- methods like 'GET' or 'OPTIONS', amount of implicit assumptions is
-- minimized by making user specify his\/her intentions in explicit form
-- (for example, it's not possible to avoid specifying body or method of a
-- request). Authentication methods that assume TLS force user to use TLS on
-- type level. The library carefully hides underlying types from lower-level
-- @http-client@ package because it's not type safe enough (for example
-- 'L.Request' is an instance of 'Data.String.IsString' and if it's
-- malformed, it will blow up at run-time).
--
-- “Expandable” refers to the ability of the library to be expanded without
-- ugly hacking. For example, it's possible to define your own HTTP methods,
-- new ways to construct body of request, new authorization options, new
-- ways to actually perform request and how to represent\/parse response. As
-- user extends the library to satisfy his\/her special needs, the new
-- solutions work just like built-ins. That said, all common cases are
-- covered by the library out-of-the-box.
--
-- “High-level” means that there are less details to worry about. The
-- library is a result of my experiences as a Haskell consultant, working
-- for several clients who have very different projects and so the library
-- adapts easily to any particular style of writing Haskell applications.
-- For example, some people prefer throwing exceptions, while others are
-- concerned with purity: just define 'handleHttpException' accordingly when
-- making your monad instance of 'MonadHttp' and it will play seamlessly.
-- Finally, the library cuts boilerplate considerably and helps write
-- concise, easy to read and maintain code.
--
-- === Using with other libraries
--
--     * You won't need low-level interface of @http-client@ most of the
--       time, but when you do, it's better import it qualified because it
--       has naming conflicts with @req@.
--     * For streaming of large request bodies see companion package
--       @req-conduit@: <https://hackage.haskell.org/package/req-conduit>.
--
-- === Lightweight, no risk solution
--
-- The library uses the following mature packages under the hood to
-- guarantee you best experience without bugs or other funny business:
--
--     * <https://hackage.haskell.org/package/http-client> — low level HTTP
--       client used everywhere in Haskell.
--     * <https://hackage.haskell.org/package/http-client-tls> — TLS (HTTPS)
--       support for @http-client@.
--
-- It's important to note that since we leverage well-known libraries that
-- the whole Haskell ecosystem uses, there is no risk in using @req@, as the
-- machinery for performing requests is the same as with @http-conduit@ and
-- @wreq@, it's just the API is different.

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Network.HTTP.Req
  ( -- * Making a request
    -- $making-a-request
    req
    -- * Embedding requests into your monad
    -- $embedding-requests
  , MonadHttp  (..)
  , HttpConfig (..)
    -- * Request
    -- ** Method
    -- $method
  , GET     (..)
  , POST    (..)
  , HEAD    (..)
  , PUT     (..)
  , DELETE  (..)
  , TRACE   (..)
  , CONNECT (..)
  , OPTIONS (..)
  , PATCH   (..)
  , HttpMethod (..)
    -- ** URL
    -- $url
  , Url
  , http
  , https
  , (/:)
  , parseUrlHttp
  , parseUrlHttps
    -- ** Body
    -- $body
  , NoReqBody (..)
  , ReqBodyJSON (..)
  , ReqBodyFile (..)
  , ReqBodyBs (..)
  , ReqBodyLbs (..)
  , ReqBodyUrlEnc (..)
  , FormUrlEncodedParam
  , HttpBody (..)
  , ProvidesBody
    -- ** Optional parameters
    -- $optional-parameters
  , Option
    -- *** Query parameters
    -- $query-parameters
  , (=:)
  , queryFlag
  , QueryParam (..)
    -- *** Headers
  , header
    -- *** Cookies
    -- $cookies
  , cookieJar
    -- *** Authentication
    -- $authentication
  , basicAuth
  , oAuth1
  , oAuth2Bearer
  , oAuth2Token
  , awsAuth
    -- *** Other
  , port
  , decompress
  , responseTimeout
  , httpVersion
    -- * Response
    -- ** Response interpretations
  , IgnoredResponseBody
    -- ** Inspecting a response
  , responseBody
  , responseStatusCode
  , responseStatusMessage
  , responseHeader
  , responseCookieJar
    -- ** Defining your own interpretation
  , HttpResponse (..)
    -- * Other
  , CanHaveBody (..)
  , Scheme (..) )
where

import Control.Applicative
import Control.Arrow (first, second)
import Control.Exception (try)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (ToJSON (..))
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Default.Class
import Data.Function (on)
import Data.IORef
import Data.List (nubBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Semigroup hiding (Option, option)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import Web.HttpApiData (ToHttpApiData (..))
import qualified Blaze.ByteString.Builder     as BB
import qualified Data.Aeson                   as A
import qualified Data.Binary.Builder          as R
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import qualified Data.CaseInsensitive         as CI
import qualified Data.List.NonEmpty           as NE
import qualified Data.Text.Encoding           as T
import qualified Network.Connection           as NC
import qualified Network.HTTP.Client          as L
import qualified Network.HTTP.Client.Internal as LI
import qualified Network.HTTP.Client.TLS      as L
import qualified Network.HTTP.Req.AWS         as AWS
import qualified Network.HTTP.Req.OAuth1      as OAuth1
import qualified Network.HTTP.Types           as Y

#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif

----------------------------------------------------------------------------
-- Making a request

-- $making-a-request
--
-- To make an HTTP request you need only one function: 'req'.

-- | Make an HTTP request.
--
-- TODO Finish docs of this function when the package is more developed.

req
  :: forall m method body response scheme.
     ( MonadHttp    m
     , HttpMethod   method
     , HttpBody     body
     , HttpResponse response
     , AllowsBody   method ~ ProvidesBody body )
  => method            -- ^ HTTP method
  -> Url scheme        -- ^ 'Url' — location of resource
  -> body              -- ^ Body of the request
  -> Option scheme     -- ^ Collection of optional parameters
  -> m response        -- ^ Response
req method url body options = do
  config  <- getHttpConfig
  manager <- liftIO (readIORef globalManager)
  let -- NOTE First appearance of any given header wins. This allows to
      -- “overwrite” headers when we construct a request by cons-ing.
      nubHeaders = Endo $ \x ->
        x { L.requestHeaders = nubBy ((==) `on` fst) (L.requestHeaders x) }
      request = flip appEndo L.defaultRequest $
      -- NOTE Order of 'mappend's matters, here method is overwritten first
      -- and 'options' take effect last. In particular, this means that
      -- 'options' can overwrite things set by other request components,
      -- which is useful for setting port number, "Content-Type" header,
      -- etc.
        nubHeaders                                        <>
        getRequestMod options                             <>
        getRequestMod config                              <>
        getRequestMod (Womb body   :: Womb "body"   body) <>
        getRequestMod url                                 <>
        getRequestMod (Womb method :: Womb "method" method)
  liftIO (try $ getHttpResponse manager request)
    >>= either handleHttpException return

-- | Global 'L.Manager' that 'req' uses. Here we just go with the default
-- settings, so users don't need to deal with this manager stuff at all, but
-- when we create a request, instance 'HttpConfig' can affect the default
-- settings via 'getHttpConfig'.
--
-- A note about safety, in case 'unsafePerformIO' looks suspicious to you.
-- The value of 'globalManager' is named and lives on top level. This means
-- it will be shared, i.e. computed only once on first use of manager. From
-- that moment on the 'IORef' will be just reused — exactly the behaviour we
-- want here in order to maximize connection sharing. GHC could spoil the
-- plan by inlining the definition, hence the @NOINLINE@ pragma.

globalManager :: IORef L.Manager
globalManager = unsafePerformIO $ do
  context <- NC.initConnectionContext
  let settings = L.mkManagerSettingsContext (Just context) def Nothing
  manager <- L.newManager settings
  newIORef manager
{-# NOINLINE globalManager #-}

----------------------------------------------------------------------------
-- Embedding requests into your monad

-- $embedding-requests
--
-- To use 'req' in your monad, all you need to do is to make the monad an
-- instance of the 'MonadHttp' type class.

-- | A type class for monads that support performing HTTP requests.
-- Typically, you only need to define the 'handleHttpException' method
-- unless you want to tweak 'HttpConfig'.

class MonadIO m => MonadHttp m where

  {-# MINIMAL handleHttpException #-}

  -- | This method describes how to deal with 'L.HttpException' that was
  -- caught by the library. One option is to re-throw it if you are OK with
  -- exceptions, but if you prefer working with something like
  -- 'Control.Monad.Error.MonadError', this is the right place to pass it to
  -- 'Control.Monad.Error.throwError' for example.

  handleHttpException :: L.HttpException -> m a

  -- | Return 'HttpConfig' to be used when performing HTTP requests. Default
  -- implementation returns its 'def' value, which is described in the
  -- documentation for the type. Common usage pattern with manually defined
  -- 'getHttpConfig' is to return some hard-coded value, or value extracted
  -- from 'Control.Monad.Reader.MonadReader' if a more flexible approach to
  -- configuration is desirable.

  getHttpConfig :: m HttpConfig
  getHttpConfig = return def

-- | 'HttpConfig' contains general and default settings to be used when
-- making HTTP requests.

data HttpConfig = HttpConfig
  { httpConfigProxy         :: Maybe L.Proxy
    -- ^ Proxy to use. By default values of @HTTP_PROXY@ and @HTTPS_PROXY@
    -- environment variables are respected, this setting overwrites them.
    -- Default value: 'Nothing'.
  , httpConfigRedirectCount :: Word
    -- ^ How many redirects to follow when getting a resource. Default
    -- value: 10.
  , httpConfigAltManager    :: Maybe L.Manager
    -- ^ Alternative 'L.Manager' to use. 'Nothing' (default value) means
    -- that default implicit manager will be used (that's what you want in
    -- 99% of cases).
  , httpConfigCheckResponse :: L.Request -> L.Response L.BodyReader -> IO ()
    -- ^ Function to check the response immediately after receiving the
    -- status and headers. This is used for throwing exceptions on
    -- non-success status codes by default (set to @\\_ _ -> return ()@ if
    -- this behavior is not desirable). Throwing is better then just
    -- returning a request with non-2xx status code because in that case
    -- something is wrong and we need a way to short-cut execution. The
    -- thrown exception is caught by the library though and is available in
    -- 'handleHttpException'.
  } deriving Typeable

instance Default HttpConfig where
  def = HttpConfig
    { httpConfigProxy         = Nothing
    , httpConfigRedirectCount = 10
    , httpConfigAltManager    = Nothing
    , httpConfigCheckResponse = \_ response ->
        let Y.Status statusCode _ = L.responseStatus response in
          unless (200 <= statusCode && statusCode < 300) $ do
            chunk <- BL.toStrict <$> L.brReadSome (L.responseBody response) 1024
            LI.throwHttp (L.StatusCodeException (void response) chunk) }

instance RequestComponent HttpConfig where
  getRequestMod HttpConfig {..} = Endo $ \x ->
    x { L.proxy                   = httpConfigProxy
      , L.redirectCount           = fromIntegral httpConfigRedirectCount
      , LI.requestManagerOverride = httpConfigAltManager
      , LI.checkResponse          = httpConfigCheckResponse }

----------------------------------------------------------------------------
-- Request — Method

-- $method
--
-- The package supports all methods as defined by RFC 2616, and 'PATCH'
-- which is defined by RFC 5789 — that should be enough to talk to RESTful
-- APIs. In some cases, however, you may want to add more methods (e.g. you
-- work with WebDAV <https://en.wikipedia.org/wiki/WebDAV>); no need to
-- compromise on type safety and hack, it only takes a couple of seconds to
-- define a new method that will works seamlessly, see 'HttpMethod'.

-- | 'GET' method.

data GET = GET

instance HttpMethod GET where
  type AllowsBody GET = 'NoBody
  httpMethodName Proxy = Y.methodGet

-- | 'POST' method.

data POST = POST

instance HttpMethod POST where
  type AllowsBody POST = 'CanHaveBody
  httpMethodName Proxy = Y.methodPost

-- | 'HEAD' method.

data HEAD = HEAD

instance HttpMethod HEAD where
  type AllowsBody HEAD = 'NoBody
  httpMethodName Proxy = Y.methodHead

-- | 'PUT' method.

data PUT = PUT

instance HttpMethod PUT where
  type AllowsBody PUT = 'CanHaveBody
  httpMethodName Proxy = Y.methodPut

-- | 'DELETE' method. This data type does not allow having request body with
-- 'DELETE' requests, as it should be, however some APIs may expect 'DELETE'
-- requests to have bodies, in that case define your own variation of
-- 'DELETE' method and allow it to have a body.

data DELETE = DELETE

instance HttpMethod DELETE where
  type AllowsBody DELETE = 'NoBody
  httpMethodName Proxy = Y.methodDelete

-- | 'TRACE' method.

data TRACE = TRACE

instance HttpMethod TRACE where
  type AllowsBody TRACE = 'CanHaveBody
  httpMethodName Proxy = Y.methodTrace

-- | 'CONNECT' method.

data CONNECT = CONNECT

instance HttpMethod CONNECT where
  type AllowsBody CONNECT = 'CanHaveBody
  httpMethodName Proxy = Y.methodConnect

-- | 'OPTIONS' method.

data OPTIONS = OPTIONS

instance HttpMethod OPTIONS where
  type AllowsBody OPTIONS = 'NoBody
  httpMethodName Proxy = Y.methodOptions

-- | 'PATCH' method.

data PATCH = PATCH

instance HttpMethod PATCH where
  type AllowsBody PATCH = 'CanHaveBody
  httpMethodName Proxy = Y.methodPatch

-- | A type class for types that can be used as an HTTP method. To define a
-- non-standard method, follow this example that defines COPY:
--
-- > data COPY = COPY
-- >
-- > instance HttpMethod COPY where
-- >   type AllowsBody COPY = 'CanHaveBody
-- >   httpMethodName Proxy = "COPY"

class HttpMethod a where

  -- | Type function 'AllowsBody' returns type of kind 'CanHaveBody' which
  -- tells the rest of the library whether the method can have a body or
  -- not. We use the special type 'CanHaveBody' “lifted” into kind instead
  -- of 'Bool' to get more user-friendly compiler messages.

  type AllowsBody a :: CanHaveBody

  -- | Return name of the method as a 'ByteString'.

  httpMethodName :: Proxy a -> ByteString

instance HttpMethod method => RequestComponent (Womb "method" method) where
  getRequestMod _ = Endo $ \x ->
    x { L.method = httpMethodName (Proxy :: Proxy method) }

----------------------------------------------------------------------------
-- Request — URL

-- $url
--
-- We use 'Url's which are correct by construction, see 'Url'. To build a
-- 'Url' from 'ByteString', use 'parseUrlHttp' or 'parseUrlHttps'.

-- | Request's 'Url'. Start constructing your 'Url' with 'http' or 'https'
-- specifying the scheme and host at the same time. Then use the @('/:')@
-- operator to grow path one piece at a time. Every single piece of path
-- will be url(percent)-encoded, so @('/:')@ is the only way to have forward
-- slashes between path segments. This approach makes working with dynamic
-- path segments easy and safe. See examples below how to represent various
-- 'Url's (make sure the @OverloadedStrings@ language extension is enabled).
--
-- ==== __Examples__
--
-- > http "httpbin.org"
-- > -- http://httpbin.org
--
-- > https "httpbin.org"
-- > -- https://httpbin.org
--
-- > https "httpbin.org" /: "encoding" /: "utf8"
-- > -- https://httpbin.org/encoding/utf8
--
-- > https "httpbin.org" /: "foo" /: "bar/baz"
-- > -- https://httpbin.org/foo/bar%2Fbaz
--
-- > https "httpbin.org" /: "bytes" /: (10 :: Int)
-- > -- https://httpbin.org/bytes/10
--
-- > https "юникод.рф"
-- > -- https://%D1%8E%D0%BD%D0%B8%D0%BA%D0%BE%D0%B4.%D1%80%D1%84

data Url (scheme :: Scheme) = Url Scheme (NonEmpty Text)
  -- NOTE The second value is path segments in reversed order.
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | Given host name, produce a 'Url' which have “http” as its scheme and
-- empty path. This also sets port to @80@.

http :: Text -> Url 'Http
http = Url Http . pure

-- | Given host name, produce a 'Url' which have “https” as its scheme and
-- empty path. This also sets port to @443@.

https :: Text -> Url 'Https
https = Url Https . pure

-- | Grow given 'Url' appending a single path segment to it. Note that the
-- path segment can be of any type that is an instance of 'ToHttpApiData'.

infixl 5 /:
(/:) :: ToHttpApiData a => Url scheme -> a -> Url scheme
Url secure path /: segment = Url secure (NE.cons (toUrlPiece segment) path)

-- | The 'parseUrlHttp' function provides an alternative method to get 'Url'
-- (possibly with some 'Option's) from a 'ByteString'. This is useful when
-- you are given a URL to query dynamically and don't know it beforehand.
-- The function parses 'ByteString' because it's the correct type to
-- represent a URL, as 'Url' cannot contain characters outside of ASCII
-- range, thus we can consider every character a 'Data.Word.Word8' value.
--
-- This function only parses 'Url' (scheme, host, path) and optional query
-- parameters that are returned as 'Option'. It does not parse method name
-- or authentication info from given 'ByteString'.

parseUrlHttp :: ByteString -> Maybe (Url 'Http, Option scheme)
parseUrlHttp url' = do
  url <- B.stripPrefix "http://" url'
  (host :| path, option) <- parseUrlHelper url
  return (foldl (/:) (http host) path, option)

-- | Just like 'parseUrlHttp', but expects “https” scheme.

parseUrlHttps :: ByteString -> Maybe (Url 'Https, Option scheme)
parseUrlHttps url' = do
  url <- B.stripPrefix "https://" url'
  (host :| path, option) <- parseUrlHelper url
  return (foldl (/:) (https host) path, option)

-- | Get host\/collection of path pieces and possibly query parameters
-- already converted to 'Option'. This function is not public.

parseUrlHelper :: ByteString -> Maybe (NonEmpty Text, Option scheme)
parseUrlHelper url = do
  let (path', query') = B.break (== 0x3f) url
      query = mconcat (uncurry queryParam <$> Y.parseQueryText query')
  path <- NE.nonEmpty (Y.decodePathSegments path')
  return (path, query)

instance RequestComponent (Url scheme) where
  getRequestMod (Url scheme segments) = Endo $ \x ->
    let (host :| path) = NE.reverse segments in
    x { L.secure = case scheme of
          Http  -> False
          Https -> True
      , L.port   = case scheme of
          Http  -> 80
          Https -> 443
      , L.host   = Y.urlEncode False (T.encodeUtf8 host)
      , L.path   =
          (BL.toStrict . R.toLazyByteString . Y.encodePathSegments) path }

----------------------------------------------------------------------------
-- Request — Body

-- $body
--
-- A number of options for request bodies are available. The @Content-Type@
-- header is set for you automatically according to body option you use
-- (it's always specified in documentation for given body option). To add
-- your own way to represent request body, see 'HttpBody'.

-- | This data type represents empty body of an HTTP request. This is the
-- data type to use with 'HttpMethod's that cannot have a body, as it's the
-- only type for which 'ProvidesBody' returns 'NoBody'.
--
-- Using of this body option does not set the @Content-Type@ header.

data NoReqBody = NoReqBody

instance HttpBody NoReqBody where
  getRequestBody NoReqBody = L.RequestBodyBS B.empty

-- | This body option allows to use a JSON object as request body — probably
-- the most popular format right now. Just wrap a data type that is an
-- instance of 'ToJSON' type class and you are done: it will be converted to
-- JSON and inserted as request body.
--
-- This body option sets the @Content-Type@ header to @\"application/json;
-- charset=utf-8\"@ value.

newtype ReqBodyJSON a = ReqBodyJSON a

instance ToJSON a => HttpBody (ReqBodyJSON a) where
  getRequestBody (ReqBodyJSON a) = L.RequestBodyLBS (A.encode a)
  getRequestContentType Proxy = pure "application/json; charset=utf-8"

-- | This body option streams request body from a file. It is expected that
-- the file size does not change during the streaming.
--
-- Using of this body option does not set the @Content-Type@ header.

newtype ReqBodyFile = ReqBodyFile FilePath

instance HttpBody ReqBodyFile where
  getRequestBody (ReqBodyFile path) =
    LI.RequestBodyIO (L.streamFile path)

-- | HTTP request body represented by a strict 'ByteString'.
--
-- Using of this body option does not set the @Content-Type@ header.

newtype ReqBodyBs = ReqBodyBs ByteString

instance HttpBody ReqBodyBs where
  getRequestBody (ReqBodyBs bs) = L.RequestBodyBS bs

-- | HTTP request body represented by a lazy 'BL.ByteString'.
--
-- Using of this body option does not set the @Content-Type@ header.

newtype ReqBodyLbs = ReqBodyLbs BL.ByteString

instance HttpBody ReqBodyLbs where
  getRequestBody (ReqBodyLbs bs) = L.RequestBodyLBS bs

-- | Form URL-encoded body. This can hold a collection of parameters which
-- are encoded similarly to query parameters at the end of query string,
-- with the only difference that they are stored in request body. The
-- similarity is reflected in API as well, as you can use the same
-- combinators you would use to add query parameters: @('=:')@ and
-- 'queryFlag'.
--
-- Here is an example of use:
--
-- > req POST (https "httpbin.org" /: "post") (ReqBodyUrlEnc params) mempty
-- >   where params =
-- >     "foo" =: "bar" <>
-- >     queryFlag "baz"
--
-- This body option sets the @Content-Type@ header to
-- @\"application/x-www-from-urlencoded\"@ value.

newtype ReqBodyUrlEnc = ReqBodyUrlEnc FormUrlEncodedParam

instance HttpBody ReqBodyUrlEnc where
  getRequestBody (ReqBodyUrlEnc (FormUrlEncodedParam params)) =
    (L.RequestBodyLBS . BB.toLazyByteString) (Y.renderQueryText False params)
  getRequestContentType Proxy = pure "application/x-www-form-urlencoded"

-- | An opaque monoidal value that allows to collect URL-encoded parameters
-- to be wrapped in 'ReqBodyUrlEnc' (documentation for this type has an
-- example of use).

newtype FormUrlEncodedParam = FormUrlEncodedParam [(Text, Maybe Text)]
  deriving (Semigroup, Monoid)

instance QueryParam FormUrlEncodedParam where
  queryParam name mvalue =
    FormUrlEncodedParam [(name, toQueryParam <$> mvalue)]

-- | A type class for things that can be interpreted as HTTP
-- 'L.RequestBody'.

class HttpBody body where

  {-# MINIMAL getRequestBody #-}

  -- | How to get actual 'L.RequestBody'.

  getRequestBody :: body -> L.RequestBody

  -- | This method allows to optionally specify value of @Content-Type@
  -- header that should be used when this body option is used. By default
  -- this returns 'Nothing' and so @Content-Type@ is not set.

  getRequestContentType :: Proxy body -> Maybe ByteString
  getRequestContentType Proxy = Nothing

-- | The type function recognizes 'NoReqBody' as having 'NoBody', while any
-- other body option 'CanHaveBody'. This forces user to use 'NoReqBody' with
-- 'GET' method and other methods that should not send a body.

type family ProvidesBody body :: CanHaveBody where
  ProvidesBody NoReqBody = 'NoBody
  ProvidesBody body      = 'CanHaveBody

instance HttpBody body => RequestComponent (Womb "body" body) where
  getRequestMod (Womb body) = Endo $ \x ->
    x { L.requestBody = getRequestBody body
      , L.requestHeaders =
        let old = L.requestHeaders x in
          case getRequestContentType (Proxy :: Proxy body) of
            Nothing -> old
            Just contentType ->
              (Y.hContentType, contentType) : old }

----------------------------------------------------------------------------
-- Request — Optional parameters

-- $optional-parameters
--
-- Optional parameters to a request include things like query parameters,
-- headers, port number, etc. All optional parameters have the type
-- 'Option', which is a 'Monoid'. This means that you can use 'mempty' as
-- the last argument of 'req' to specify no optional parameters, or combine
-- 'Option's using 'mappend' (or @('<>')@) to have several of them at once.

-- | Opaque 'Option' type is a 'Monoid' you can use to pack collection of
-- optional parameters like query parameters and headers. See sections below
-- to learn which 'Option' primitives are available.

-- TODO We need examples here.

data Option (scheme :: Scheme) =
  Option (Endo (Y.QueryText, L.Request)) (Maybe (Endo L.Request))
  -- NOTE 'QueryText' is just [(Text, Maybe Text)], we keep it along with
  -- Request to avoid appending to existing query string in request every
  -- time new parameter is added. Additional Maybe (Endo Request) is a
  -- finalizer that will be applied after all over options. This is for
  -- authentication methods that sign requests based on data in Request.

instance Semigroup (Option scheme) where
  Option er0 mr0 <> Option er1 mr1 = Option
    (er0 <> er1) (mr0 <|> mr1)

instance Monoid (Option scheme) where
  mempty  = Option mempty Nothing
  mappend = (<>)

-- | A helper to create an 'Option' that modifies only collection of query
-- parameters. This helper is not a part of public API.

withQueryParams :: (Y.QueryText -> Y.QueryText) -> Option scheme
withQueryParams f = Option (Endo (first f)) Nothing

-- | A helper to create an 'Option' that modifies only 'L.Request'. This
-- helper is not a part of public API.

withRequest :: (L.Request -> L.Request) -> Option scheme
withRequest f = Option (Endo (second f)) Nothing

-- | A helper to create an 'Option' that adds a finalizer (request
-- endomorphism that is run after all other modifications).

asFinalizer :: (L.Request -> L.Request) -> Option scheme
asFinalizer f = Option mempty (Just (Endo f))

instance RequestComponent (Option scheme) where
  getRequestMod (Option f finalizer) = Endo $ \x ->
    let (qparams, x') = appEndo f ([], x)
        query         = Y.renderQuery True (Y.queryTextToQuery qparams)
    in maybe id appEndo finalizer x' { L.queryString = query }

----------------------------------------------------------------------------
-- Request — Optional parameters — Query Parameters

-- $query-parameters
--
-- This section describes a polymorphic interface that can be used to
-- construct query parameters (of type 'Option') and form URL-encoded bodies
-- (of type 'FormUrlEncodedParam').

-- | This operator builds a query parameter that will be included in URL of
-- your request after question sign @?@. This is the same syntax you use
-- with form URL encoded request bodies.
--
-- This operator is defined in terms of 'queryParam':
--
-- > name =: value = queryParam name (pure value)

infix 7 =:
(=:) :: (QueryParam param, ToHttpApiData a) => Text -> a -> param
name =: value = queryParam name (pure value)

-- | Construct a flag, that is, valueless query parameter. For example, in
-- the following URL @a@ is a flag, @b@ is a query parameter with a value:
--
-- > https://httpbin.org/foo/bar?a&b=10
--
-- This operator is defined in terms of 'queryParam':
--
-- > queryFlag name = queryParam name Nothing

queryFlag :: QueryParam param => Text -> param
queryFlag name = queryParam name (Nothing :: Maybe ())

-- | A type class for query-parameter-like things. The reason to have
-- overloaded 'queryParam' is to be able to use it as an 'Option' and as a
-- 'FormUrlEncodedParam' when constructing form URL encoded request bodies.
-- Having the same syntax for these cases seems natural and user-friendly.

class QueryParam param where

  -- | Create a query parameter with given name and value. If value is
  -- 'Nothing', it won't be included at all (i.e. you create a flag this
  -- way). It's recommended to use @('=:')@ and 'queryFlag' instead of this
  -- method, because they are easier to read.

  queryParam :: ToHttpApiData a => Text -> Maybe a -> param

instance QueryParam (Option scheme) where
  queryParam name mvalue =
    withQueryParams ((:) (name, toQueryParam <$> mvalue))

----------------------------------------------------------------------------
-- Request — Optional parameters — Headers

-- | Create an 'Option' that adds a header.

header
  :: ByteString        -- ^ Header name
  -> ByteString        -- ^ Header value
  -> Option scheme
header name value = withRequest (attachHeader name value)

-- | A non-public helper that attaches a header with given name and content
-- to a 'L.Request'.

attachHeader :: ByteString -> ByteString -> L.Request -> L.Request
attachHeader name value x =
  x { L.requestHeaders = (CI.mk name, value) : L.requestHeaders x }

----------------------------------------------------------------------------
-- Request — Optional parameters — Cookies

-- $cookies
--
-- Support for cookies is quite minimalistic at the moment, its' possible to
-- specify which cookies to send using 'cookieJar' and inspect 'L.Response'
-- to extract 'L.CookieJar' from it.

-- | Use the given 'L.CookieJar'. A 'L.CookieJar' can be obtained from a
-- 'L.Response' record.

cookieJar :: L.CookieJar -> Option scheme
cookieJar jar = withRequest $ \x ->
  x { L.cookieJar = Just jar }

----------------------------------------------------------------------------
-- Request — Optional parameters — Authentication

-- $authentication
--
-- This section provides common authentication helpers in form of 'Option's.
-- You should always prefer the provided authentication 'Option's to manual
-- construction of headers because it ensures that you only use one
-- authentication method at a time (they overwrite each other) and provides
-- additional type safety that prevents leaking of credentials in cases when
-- authentication relies on TLS for encrypting sensitive data.

-- | OAuth 1 authentication 'Option'.
--
-- See also: <https://en.wikipedia.org/wiki/OAuth>

oAuth1
  :: ByteString        -- ^ Consumer token
  -> ByteString        -- ^ Consumer secret
  -> ByteString        -- ^ OAuth token
  -> ByteString        -- ^ OAuth token secret
  -> Option scheme
oAuth1 ctoken csecret otoken osecret = asFinalizer
  (OAuth1.signRequest ctoken csecret otoken osecret)

-- | The 'Option' adds basic authentication. The 'Text' values will be UTF-8
-- encoded.
--
-- See also: <https://en.wikipedia.org/wiki/Basic_access_authentication>

basicAuth
  :: ByteString        -- ^ Username
  -> ByteString        -- ^ Password
  -> Option 'Https     -- ^ Auth 'Option'
basicAuth username password = asFinalizer
  (L.applyBasicAuth username password)

-- | The 'Option' adds an OAuth2 bearer token. This is treated by many
-- services as the equivalent of a username and password.
--
-- The 'Option' is defined as:
--
-- > oAuth2Bearer token = header "Authorization" ("Bearer " <> token)
--
-- See also: <https://en.wikipedia.org/wiki/OAuth>

oAuth2Bearer
  :: ByteString        -- ^ Token
  -> Option 'Https     -- ^ Auth 'Option'
oAuth2Bearer token = asFinalizer
  (attachHeader "Authorization" ("Bearer " <> token))

-- | The 'Option' adds a not-quite-standard OAuth2 bearer token (that seems
-- to be used only by GitHub). This will be treated by whatever services
-- accept it as the equivalent of a username and password.
--
-- The 'Option' is defined as:
--
-- > oAuth2Token token = header "Authorization" ("token" <> token)
--
-- See also: <https://developer.github.com/v3/oauth#3-use-the-access-token-to-access-the-api>

oAuth2Token
  :: ByteString        -- ^ Token
  -> Option 'Https     -- ^ Auth 'Option'
oAuth2Token token = asFinalizer
  (attachHeader "Authorization" ("token" <> token))

-- | The 'Option' adds AWS v4 request signature.
--
-- See also: <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html>

awsAuth
  :: ByteString        -- ^ AWS access key
  -> ByteString        -- ^ AWS secret access key
  -> Option 'Https     -- ^ Auth 'Option'
awsAuth accessKey secretKey = asFinalizer
  (AWS.signRequest accessKey secretKey)

----------------------------------------------------------------------------
-- Request — Optional parameters — Other

-- | Specify the port to connect to explicitly. Normally, 'Url' you use
-- determines default port, @80@ for HTTP and @443@ for HTTPS, this 'Option'
-- allows to choose arbitrary port overwriting the defaults.

port :: Word -> Option scheme
port n = withRequest $ \x ->
  x { L.port = fromIntegral n }

-- | This 'Option' controls whether gzipped data should be decompressed on
-- the fly. By default everything except for @application\/x-tar@ is
-- decompressed, i.e. we have:
--
-- > decompress (/= "application/x-tar")
--
-- You can also choose to decompress everything like this:
--
-- > decompress (const True)

decompress
  :: (ByteString -> Bool) -- ^ Predicate that is given MIME type, it
     -- returns 'True' when content should be decompressed on the fly.
  -> Option scheme
decompress f = withRequest $ \x ->
  x { L.decompress = f }

-- | Specify number of microseconds to wait for response. Default is 30
-- seconds.

responseTimeout
  :: Word              -- ^ Number of microseconds to wait
  -> Option scheme
responseTimeout n = withRequest $ \x ->
  x { L.responseTimeout = LI.ResponseTimeoutMicro (fromIntegral n) }

-- | HTTP version to send to server, default is HTTP 1.1.

httpVersion
  :: Word              -- ^ Major version number
  -> Word              -- ^ Minor version number
  -> Option scheme
httpVersion major minor = withRequest $ \x ->
  x { L.requestVersion =
        Y.HttpVersion (fromIntegral major) (fromIntegral minor) }

----------------------------------------------------------------------------
-- Response interpretations

data IgnoredResponseBody = IgnoredResponseBody (L.Response ())

----------------------------------------------------------------------------
-- Inspecting a response

-- | Get response body.

responseBody
  :: HttpResponse response
  => response
  -> HttpResponseBody response
responseBody = L.responseBody . toVanillaResponse

-- | Get response status code.

responseStatusCode
  :: HttpResponse response
  => response
  -> Word
responseStatusCode =
  fromIntegral . Y.statusCode . L.responseStatus . toVanillaResponse

-- | Get response status message.

responseStatusMessage
  :: HttpResponse response
  => response
  -> ByteString
responseStatusMessage =
  Y.statusMessage . L.responseStatus . toVanillaResponse

-- | Look a particular header from a response.

responseHeader
  :: HttpResponse response
  => response          -- ^ Response interpretation
  -> ByteString        -- ^ Header to lookup
  -> Maybe ByteString  -- ^ Header value if found
responseHeader r h =
  (lookup (CI.mk h) . L.responseHeaders . toVanillaResponse) r

-- | Get response 'L.CookieJar'.

responseCookieJar
  :: HttpResponse response
  => response
  -> L.CookieJar
responseCookieJar = L.responseCookieJar . toVanillaResponse

----------------------------------------------------------------------------
-- Response — defining your own interpretation

-- | Here we need to provide various options how to consume responses.

class HttpResponse response where

  type HttpResponseBody response :: *

  toVanillaResponse :: response -> L.Response (HttpResponseBody response)

  getHttpResponse :: L.Manager -> L.Request -> IO response

----------------------------------------------------------------------------
-- Other

-- | The main class for things that are “parts” of 'L.Request' in the sense
-- that if we have a 'L.Request', then we know how to apply an instance of
-- 'RequestComponent' changing\/overwriting something in it. 'Endo' is
-- endomorphism of functions under composition, it's used to chain different
-- request component easier using @('<>')@.

class RequestComponent a where

  -- | Get a function that takes a 'L.Request' and changes it somehow
  -- returning another 'L.Request'. For example HTTP method instance of
  -- 'RequestComponent' just overwrites method. The function is wrapped in
  -- 'Endo' so it's easier to chain such “modifying applications” together
  -- building bigger and bigger 'RequestComponent's.

  getRequestMod :: a -> Endo L.Request

-- | This wrapper is only used to attach a type-level tag to given type.
-- This is necessary to define instances of 'RequestComponent' for any thing
-- that implements 'HttpMethod' or 'HttpBody'. Without the tag, GHC is not
-- able to see difference between @'HttpMethod' method => 'RequestComponent'
-- method@ and @'HttpBody' body => 'RequestComponent' body@ when it decides
-- which instance to use (i.e. constraints are taken into account later,
-- when instance is already chosen).

newtype Womb (tag :: Symbol) a = Womb a

-- | A simple 'Bool'-like type we only have for better error messages. We
-- use it as a kind and its data constructors as type-level tags.
--
-- See also: 'HttpMethod' and 'HttpBody'.

data CanHaveBody
  = CanHaveBody        -- ^ Indeed can have a body
  | NoBody             -- ^ Should not have a body

-- | A type-level tag that specifies URL scheme used (and thus if TLS is
-- enabled). This is used to force TLS requirement for some authentication
-- 'Option's.

data Scheme
  = Http               -- ^ HTTP, no TLS
  | Https              -- ^ HTTPS
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
