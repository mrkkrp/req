-- |
-- Module      :  Network.HTTP.Req
-- Copyright   :  © 2016–2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The documentation below is structured in such a way that the most
-- important information is presented first: you learn how to do HTTP
-- requests, how to embed them in any monad you have, and then it gives you
-- details about less-common things you may want to know about. The
-- documentation is written with sufficient coverage of details and
-- examples, and it's designed to be a complete tutorial on its own.
--
-- /(A modest intro goes here, click on 'req' to start making requests.)/
--
-- === About the library
--
-- Req is an easy-to-use, type-safe, expandable, high-level HTTP library
-- that just works without any fooling around.
--
-- What does the phrase “easy-to-use” mean? It means that the library is
-- designed to be beginner-friendly so it's simple to add to your monad
-- stack, intuitive to work with, well-documented, and does not get in your
-- way. Doing HTTP requests is a common task and a Haskell library for this
-- should be very approachable and clear to beginners, thus certain
-- compromises were made. For example, one cannot currently modify
-- 'L.ManagerSettings' of the default manager because the library always
-- uses the same implicit global manager for simplicity and maximal
-- connection sharing. There is a way to use your own manager with different
-- settings, but it requires a bit more typing.
--
-- “Type-safe” means that the library is protective and eliminates certain
-- classes of errors. For example, we have correct-by-construction 'Url's,
-- it's guaranteed that the user does not send the request body when using
-- methods like 'GET' or 'OPTIONS', and the amount of implicit assumptions
-- is minimized by making the user specify his\/her intentions in an
-- explicit form (for example, it's not possible to avoid specifying the
-- body or method of a request). Authentication methods that assume HTTPS
-- force the user to use HTTPS at the type level. The library also carefully
-- hides underlying types from the lower-level @http-client@ package because
-- those types are not safe enough (for example 'L.Request' is an instance
-- of 'Data.String.IsString' and, if it's malformed, it will blow up at
-- run-time).
--
-- “Expandable” refers to the ability of the library to be expanded without
-- having to resort to ugly hacking. For example, it's possible to define
-- your own HTTP methods, create new ways to construct the body of a
-- request, create new authorization options, perform a request in a
-- different way, and create your own methods to parse and represent a
-- response. As a user extends the library to satisfy his\/her special
-- needs, the new solutions will work just like the built-ins. However, all
-- of the common cases are also covered by the library out-of-the-box.
--
-- “High-level” means that there are less details to worry about. The
-- library is a result of my experiences as a Haskell consultant. Working
-- for several clients, who had very different projects, showed me that the
-- library should adapt easily to any particular style of writing Haskell
-- applications. For example, some people prefer throwing exceptions, while
-- others are concerned with purity. Just define 'handleHttpException'
-- accordingly when making your monad instance of 'MonadHttp' and it will
-- play together seamlessly. Finally, the library cuts boilerplate down
-- considerably, and helps you write concise, easy to read, and maintainable
-- code.
--
-- === Using with other libraries
--
--     * You won't need the low-level interface of @http-client@ most of the
--       time, but when you do, it's better to do a qualified import,
--       because @http-client@ has naming conflicts with @req@.
--     * For streaming of large request bodies see the companion package
--       @req-conduit@: <https://hackage.haskell.org/package/req-conduit>.
--
-- === Lightweight, no risk solution
--
-- The library uses the following mature packages under the hood to
-- guarantee you the best experience:
--
--     * <https://hackage.haskell.org/package/http-client>—low level HTTP
--       client used everywhere in Haskell.
--     * <https://hackage.haskell.org/package/http-client-tls>—TLS (HTTPS)
--       support for @http-client@.
--
-- It's important to note that since we leverage well-known libraries that
-- the whole Haskell ecosystem uses, there is no risk in using @req@. The
-- machinery for performing requests is the same as with @http-conduit@ and
-- @wreq@. The only difference is the API.

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

#if MIN_VERSION_base(4,9,0)
{-# LANGUAGE UndecidableInstances       #-}
#endif

#if __GLASGOW_HASKELL__ <  710
{-# LANGUAGE ConstraintKinds            #-}
#endif

#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Network.HTTP.Req
  ( -- * Making a request
    -- $making-a-request
    req
  , req'
  , withReqManager
    -- * Embedding requests into your monad
    -- $embedding-requests
  , MonadHttp  (..)
  , HttpConfig (..)
  , Req
  , runReq
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
  , (/~)
  , (/:)
  , parseUrlHttp
  , parseUrlHttps
    -- ** Body
    -- $body
  , NoReqBody (..)
  , ReqBodyJson (..)
  , ReqBodyFile (..)
  , ReqBodyBs (..)
  , ReqBodyLbs (..)
  , ReqBodyUrlEnc (..)
  , FormUrlEncodedParam
  , ReqBodyMultipart
  , reqBodyMultipart
  , HttpBody (..)
  , ProvidesBody
  , HttpBodyAllowed
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
  , basicAuthUnsafe
  , oAuth1
  , oAuth2Bearer
  , oAuth2Token
    -- *** Other
  , port
  , decompress
  , responseTimeout
  , httpVersion
    -- * Response
    -- ** Response interpretations
  , IgnoreResponse
  , ignoreResponse
  , JsonResponse
  , jsonResponse
  , BsResponse
  , bsResponse
  , LbsResponse
  , lbsResponse
    -- ** Inspecting a response
  , responseBody
  , responseStatusCode
  , responseStatusMessage
  , responseHeader
  , responseCookieJar
    -- ** Defining your own interpretation
    -- $new-response-interpretation
  , HttpResponse (..)
    -- * Other
  , HttpException (..)
  , CanHaveBody (..)
  , Scheme (..) )
where

import Control.Applicative
import Control.Arrow (first, second)
import Control.Exception (Exception, try, handle, throwIO)
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Retry
import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Default.Class
import Data.Function (on)
import Data.IORef
import Data.List (nubBy)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
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
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import qualified Data.CaseInsensitive         as CI
import qualified Data.List.NonEmpty           as NE
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.Read               as TR
import qualified Network.Connection           as NC
import qualified Network.HTTP.Client          as L
import qualified Network.HTTP.Client.Internal as LI
import qualified Network.HTTP.Client.MultipartFormData as LM
import qualified Network.HTTP.Client.TLS      as L
import qualified Network.HTTP.Types           as Y
import qualified Web.Authenticate.OAuth       as OAuth

#if MIN_VERSION_base(4,9,0)
import Data.Kind (Constraint)
#else
import GHC.Exts (Constraint)
#endif

----------------------------------------------------------------------------
-- Making a request

-- $making-a-request
--
-- To make an HTTP request you need only one function: 'req'.

-- | Make an HTTP request. The function takes 5 arguments, 4 of which
-- specify required parameters and the final 'Option' argument is a
-- collection of optional parameters.
--
-- Let's go through all the arguments first: @req method url body response
-- options@.
--
-- @method@ is an HTTP method such as 'GET' or 'POST'. The documentation has
-- a dedicated section about HTTP methods below.
--
-- @url@ is a 'Url' that describes location of resource you want to interact
-- with.
--
-- @body@ is a body option such as 'NoReqBody' or 'ReqBodyJson'. The
-- tutorial has a section about HTTP bodies, but usage is very
-- straightforward and should be clear from the examples below.
--
-- @response@ is a type hint how to make and interpret response of an HTTP
-- request. Out-of-the-box it can be the following:
--
--     * 'ignoreResponse'
--     * 'jsonResponse'
--     * 'bsResponse' (to get a strict 'ByteString')
--     * 'lbsResponse' (to get a lazy 'BL.ByteString')
--
-- Finally, @options@ is a 'Monoid' that holds a composite 'Option' for all
-- other optional settings like query parameters, headers, non-standard port
-- number, etc. There are quite a few things you can put there, see the
-- corresponding section in the documentation. If you don't need anything at
-- all, pass 'mempty'.
--
-- __Note__ that if you use 'req' to do all your requests, connection
-- sharing and reuse is done for you automatically.
--
-- See the examples below to get on the speed quickly.
--
-- ==== __Examples__
--
-- First, this is a piece of boilerplate that should be in place before you
-- try the examples:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main (main) where
-- >
-- > import Control.Exception (throwIO)
-- > import Control.Monad
-- > import Data.Aeson
-- > import Data.Maybe (fromJust)
-- > import Data.Monoid ((<>))
-- > import Data.Text (Text)
-- > import GHC.Generics
-- > import Network.HTTP.Req
-- > import qualified Data.ByteString.Char8 as B
-- >
-- > instance MonadHttp IO where
-- >   handleHttpException = throwIO
--
-- We will be making requests against the <https://httpbin.org> service.
--
-- Make a GET request, grab 5 random bytes:
--
-- > main :: IO ()
-- > main = do
-- >   let n :: Int
-- >       n = 5
-- >   bs <- req GET (https "httpbin.org" /: "bytes" /~ n) NoReqBody bsResponse mempty
-- >   B.putStrLn (responseBody bs)
--
-- The same, but now we use a query parameter named @\"seed\"@ to control
-- seed of the generator:
--
-- > main :: IO ()
-- > main = do
-- >   let n, seed :: Int
-- >       n    = 5
-- >       seed = 100
-- >   bs <- req GET (https "httpbin.org" /: "bytes" /~ n) NoReqBody bsResponse $
-- >     "seed" =: seed
-- >   B.putStrLn (responseBody bs)
--
-- POST JSON data and get some info about the POST request:
--
-- > data MyData = MyData
-- >   { size  :: Int
-- >   , color :: Text
-- >   } deriving (Show, Generic)
-- >
-- > instance ToJSON MyData
-- > instance FromJSON MyData
-- >
-- > main :: IO ()
-- > main = do
-- >   let myData = MyData
-- >         { size  = 6
-- >         , color = "Green" }
-- >   v <- req POST (https "httpbin.org" /: "post") (ReqBodyJson myData) jsonResponse mempty
-- >   print (responseBody v :: Value)
--
-- Sending URL-encoded body:
--
-- > main :: IO ()
-- > main = do
-- >   let params =
-- >         "foo" =: ("bar" :: Text) <>
-- >         queryFlag "baz"
-- >   response <- req POST (https "httpbin.org" /: "post") (ReqBodyUrlEnc params) jsonResponse mempty
-- >   print (responseBody response :: Value)
--
-- Using various optional parameters and URL that is not known in advance:
--
-- > main :: IO ()
-- > main = do
-- >   -- This is an example of what to do when URL is given dynamically. Of
-- >   -- course in a real application you may not want to use 'fromJust'.
-- >   let (url, options) = fromJust (parseUrlHttps "https://httpbin.org/get?foo=bar")
-- >   response <- req GET url NoReqBody jsonResponse $
-- >     "from" =: (15 :: Int)           <>
-- >     "to"   =: (67 :: Int)           <>
-- >     basicAuth "username" "password" <>
-- >     options                         <> -- contains the ?foo=bar part
-- >     port 443 -- here you can put any port of course
-- >   print (responseBody response :: Value)

req
  :: ( MonadHttp    m
     , HttpMethod   method
     , HttpBody     body
     , HttpResponse response
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body) )
  => method            -- ^ HTTP method
  -> Url scheme        -- ^ 'Url'—location of resource
  -> body              -- ^ Body of the request
  -> Proxy response    -- ^ A hint how to interpret response
  -> Option scheme     -- ^ Collection of optional parameters
  -> m response        -- ^ Response
req method url body Proxy options = req' method url body options $ \request manager -> do
  HttpConfig {..}  <- getHttpConfig
  let wrappingVanilla = handle (throwIO . VanillaHttpException)
      wrapExc         = handle (throwIO . LI.toHttpException request)
  (liftIO . try . wrappingVanilla . wrapExc) (do
    response <- retrying httpConfigRetryPolicy httpConfigRetryJudge
      (const $ getHttpResponse request manager)
    httpConfigCheckResponse request response
    return response)
    >>= either handleHttpException return

-- | Mostly like 'req' with respect to its arguments, but accepts a callback
-- that allows to perform a request in arbitrary fashion.
--
-- This function /does not/ perform handling\/wrapping exceptions, checking
-- response, and retrying. It only prepares 'L.Request' and allows you to
-- use it.
--
-- @since 0.3.0

req'
  :: forall m method body scheme a.
     ( MonadHttp  m
     , HttpMethod method
     , HttpBody   body
     , HttpBodyAllowed (AllowsBody method) (ProvidesBody body) )
  => method            -- ^ HTTP method
  -> Url scheme        -- ^ 'Url'—location of resource
  -> body              -- ^ Body of the request
  -> Option scheme     -- ^ Collection of optional parameters
  -> (L.Request -> L.Manager -> m a) -- ^ How to perform request
  -> m a
req' method url body options m = do
  config@HttpConfig {..}  <- getHttpConfig
  manager <- liftIO (readIORef globalManager)
  let -- NOTE First appearance of any given header wins. This allows to
      -- “overwrite” headers when we construct a request by cons-ing.
      nubHeaders = Endo $ \x ->
        x { L.requestHeaders = nubBy ((==) `on` fst) (L.requestHeaders x) }
      request' = flip appEndo L.defaultRequest $
        -- NOTE The order of 'mappend's matters, here method is overwritten
        -- first and 'options' take effect last. In particular, this means
        -- that 'options' can overwrite things set by other request
        -- components, which is useful for setting port number,
        -- "Content-Type" header, etc.
        nubHeaders                                        <>
        getRequestMod options                             <>
        getRequestMod config                              <>
        getRequestMod (Womb body   :: Womb "body"   body) <>
        getRequestMod url                                 <>
        getRequestMod (Womb method :: Womb "method" method)
  request <- finalizeRequest options request'
  m request manager

-- | Global 'L.Manager' that 'req' uses. Here we just go with the default
-- settings, so users don't need to deal with this manager stuff at all, but
-- when we create a request, instance 'HttpConfig' can affect the default
-- settings via 'getHttpConfig'.
--
-- A note about safety, in case 'unsafePerformIO' looks suspicious to you.
-- The value of 'globalManager' is named and lives on top level. This means
-- it will be shared, i.e. computed only once on the first use of the
-- manager. From that moment on the 'IORef' will be just reused—exactly the
-- behavior we want here in order to maximize connection sharing. GHC could
-- spoil the plan by inlining the definition, hence the @NOINLINE@ pragma.

globalManager :: IORef L.Manager
globalManager = unsafePerformIO $ do
  context <- NC.initConnectionContext
  let settings = L.mkManagerSettingsContext (Just context) def Nothing
  manager <- L.newManager settings
  newIORef manager
{-# NOINLINE globalManager #-}

-- | Perform an action using global implicit 'L.Manager' that the rest of
-- the library uses. This allows to reuse connections that the 'L.Manager'
-- controls.

withReqManager :: MonadIO m => (L.Manager -> m a) -> m a
withReqManager m = liftIO (readIORef globalManager) >>= m

----------------------------------------------------------------------------
-- Embedding requests into your monad

-- $embedding-requests
--
-- To use 'req' in your monad, all you need to do is to make the monad an
-- instance of the 'MonadHttp' type class.
--
-- When writing a library, keep your API polymorphic in terms of
-- 'MonadHttp', only define instance of 'MonadHttp' in final application.
-- Another option is to use @newtype@ wrapped monad stack and define
-- 'MonadHttp' for it. As of version /0.4.0/, the 'Req' monad is provided
-- for this out-of-the-box.

-- | A type class for monads that support performing HTTP requests.
-- Typically, you only need to define the 'handleHttpException' method
-- unless you want to tweak 'HttpConfig'.

class MonadIO m => MonadHttp m where

  -- | This method describes how to deal with 'HttpException' that was
  -- caught by the library. One option is to re-throw it if you are OK with
  -- exceptions, but if you prefer working with something like
  -- 'Control.Monad.Except.MonadError', this is the right place to pass it to
  -- 'Control.Monad.Except.throwError'.

  handleHttpException :: HttpException -> m a

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
  , httpConfigRedirectCount :: Int
    -- ^ How many redirects to follow when getting a resource. Default
    -- value: 10.
  , httpConfigAltManager    :: Maybe L.Manager
    -- ^ Alternative 'L.Manager' to use. 'Nothing' (default value) means
    -- that default implicit manager will be used (that's what you want in
    -- 99% of cases).
  , httpConfigCheckResponse :: forall r. HttpResponse r => L.Request -> r -> IO ()
    -- ^ Function to check the response immediately after receiving the
    -- status and headers. This is used for throwing exceptions on
    -- non-success status codes by default (set to @\\_ _ -> return ()@ if
    -- this behavior is not desirable). Throwing is better then just
    -- returning a request with non-2xx status code because in that case
    -- something is wrong and we need a way to short-cut execution. The
    -- thrown exception is caught by the library though and is available in
    -- 'handleHttpException'.
    --
    -- @since 0.3.0
  , httpConfigRetryPolicy :: RetryPolicyM IO
    -- ^ The retry policy to use for request retrying. By default 'def' is
    -- used (see 'RetryPolicyM').
    --
    -- @since 0.3.0
  , httpConfigRetryJudge :: forall r. HttpResponse r => RetryStatus -> r -> IO Bool
    -- ^ The function is used to decide whether to retry a request. 'True'
    -- means that the request should be retried.
    --
    -- @since 0.3.0
  } deriving Typeable

instance Default HttpConfig where
  def = HttpConfig
    { httpConfigProxy         = Nothing
    , httpConfigRedirectCount = 10
    , httpConfigAltManager    = Nothing
    , httpConfigCheckResponse = \_ response ->
        let statusCode = responseStatusCode response in
          unless (200 <= statusCode && statusCode < 300) $
            let chunk = makeResponseBodyPreview response
                vresponse = toVanillaResponse response
            in LI.throwHttp (L.StatusCodeException (void vresponse) chunk)
    , httpConfigRetryPolicy  = def
    , httpConfigRetryJudge   = \_ r -> return $
        responseStatusCode r `elem`
          [ 408 -- Request timeout
          , 504 -- Gateway timeout
          , 524 -- A timeout occurred
          , 598 -- (Informal convention) Network read timeout error
          , 599 -- (Informal convention) Network connect timeout error
          ]
    }

instance RequestComponent HttpConfig where
  getRequestMod HttpConfig {..} = Endo $ \x ->
    x { L.proxy                   = httpConfigProxy
      , L.redirectCount           = httpConfigRedirectCount
      , LI.requestManagerOverride = httpConfigAltManager }

-- | A monad that allows to run 'req' in any 'IO'-enabled monad without
-- having to define new instances.
--
-- @since 0.4.0

newtype Req a = Req (ReaderT HttpConfig IO a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO )

instance MonadBase IO Req where
  liftBase = liftIO

instance MonadBaseControl IO Req where
  type StM Req a = a
  liftBaseWith f = Req . ReaderT $ \r -> f (runReq r)
  {-# INLINEABLE liftBaseWith #-}
  restoreM       = Req . ReaderT . const . return
  {-# INLINEABLE restoreM #-}

instance MonadHttp Req where
  handleHttpException = Req . lift . throwIO
  getHttpConfig       = Req ask

-- | Run a computation in the 'Req' monad with the given 'HttpConfig'. In
-- case of exceptional situation an 'HttpException' will be thrown.
--
-- @since 0.4.0

runReq :: MonadIO m
  => HttpConfig        -- ^ 'HttpConfig' to use
  -> Req a             -- ^ Computation to run
  -> m a
runReq config (Req m) = liftIO (runReaderT m config)

----------------------------------------------------------------------------
-- Request—Method

-- $method
--
-- The package supports all methods as defined by RFC 2616, and 'PATCH'
-- which is defined by RFC 5789—that should be enough to talk to RESTful
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
-- 'DELETE' requests, as it should be. However some APIs may expect 'DELETE'
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

  -- | Type function 'AllowsBody' returns a type of kind 'CanHaveBody' which
  -- tells the rest of the library whether the method can have a body or
  -- not. We use the special type 'CanHaveBody' “lifted” to kind level
  -- instead of 'Bool' to get more user-friendly compiler messages.

  type AllowsBody a :: CanHaveBody

  -- | Return name of the method as a 'ByteString'.

  httpMethodName :: Proxy a -> ByteString

instance HttpMethod method => RequestComponent (Womb "method" method) where
  getRequestMod _ = Endo $ \x ->
    x { L.method = httpMethodName (Proxy :: Proxy method) }

----------------------------------------------------------------------------
-- Request—URL

-- $url
--
-- We use 'Url's which are correct by construction, see 'Url'. To build a
-- 'Url' from a 'ByteString', use 'parseUrlHttp' or 'parseUrlHttps'.

-- | Request's 'Url'. Start constructing your 'Url' with 'http' or 'https'
-- specifying the scheme and host at the same time. Then use the @('/~')@
-- and @('/:')@ operators to grow the path one piece at a time. Every single
-- piece of path will be url(percent)-encoded, so using @('/~')@ and
-- @('/:')@ is the only way to have forward slashes between path segments.
-- This approach makes working with dynamic path segments easy and safe. See
-- examples below how to represent various 'Url's (make sure the
-- @OverloadedStrings@ language extension is enabled).
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
-- > https "httpbin.org" /: "bytes" /~ (10 :: Int)
-- > -- https://httpbin.org/bytes/10
--
-- > https "юникод.рф"
-- > -- https://%D1%8E%D0%BD%D0%B8%D0%BA%D0%BE%D0%B4.%D1%80%D1%84

data Url (scheme :: Scheme) = Url Scheme (NonEmpty Text)
  -- NOTE The second value is the path segments in reversed order.
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

infixl 5 /~
(/~) :: ToHttpApiData a => Url scheme -> a -> Url scheme
Url secure path /~ segment = Url secure (NE.cons (toUrlPiece segment) path)

-- | Type-constrained version of @('/~')@ to remove ambiguity in the cases
-- when next URL piece is a 'Text' literal.

infixl 5 /:
(/:) :: Url scheme -> Text -> Url scheme
(/:) = (/~)

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
  p' :| ps <- NE.nonEmpty (Y.decodePathSegments path')
  (p, port') <-
    case T.break (== ':') p' of
      (x, "") -> return (x, mempty)
      (x, prt) ->
        case TR.decimal (T.drop 1 prt) of
          Right (prt',"") -> return (x, port prt')
          _               -> Nothing
  return (p :| ps, query <> port')

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
          (BL.toStrict . BB.toLazyByteString . Y.encodePathSegments) path }

----------------------------------------------------------------------------
-- Request—Body

-- $body
--
-- A number of options for request bodies are available. The @Content-Type@
-- header is set for you automatically according to the body option you use
-- (it's always specified in documentation for a given body option). To add
-- your own way to represent request body, see 'HttpBody'.

-- | This data type represents empty body of an HTTP request. This is the
-- data type to use with 'HttpMethod's that cannot have a body, as it's the
-- only type for which 'ProvidesBody' returns 'NoBody'.
--
-- Using of this body option does not set the @Content-Type@ header.

data NoReqBody = NoReqBody

instance HttpBody NoReqBody where
  getRequestBody NoReqBody = L.RequestBodyBS B.empty

-- | This body option allows to use a JSON object as request body—probably
-- the most popular format right now. Just wrap a data type that is an
-- instance of 'ToJSON' type class and you are done: it will be converted to
-- JSON and inserted as request body.
--
-- This body option sets the @Content-Type@ header to @\"application/json;
-- charset=utf-8\"@ value.

newtype ReqBodyJson a = ReqBodyJson a

instance ToJSON a => HttpBody (ReqBodyJson a) where
  getRequestBody (ReqBodyJson a) = L.RequestBodyLBS (A.encode a)
  getRequestContentType _ = pure "application/json; charset=utf-8"

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
-- similarity is reflected in the API as well, as you can use the same
-- combinators you would use to add query parameters: @('=:')@ and
-- 'queryFlag'.
--
-- This body option sets the @Content-Type@ header to
-- @\"application/x-www-form-urlencoded\"@ value.

newtype ReqBodyUrlEnc = ReqBodyUrlEnc FormUrlEncodedParam

instance HttpBody ReqBodyUrlEnc where
  getRequestBody (ReqBodyUrlEnc (FormUrlEncodedParam params)) =
    (L.RequestBodyLBS . BB.toLazyByteString) (Y.renderQueryText False params)
  getRequestContentType _ = pure "application/x-www-form-urlencoded"

-- | An opaque monoidal value that allows to collect URL-encoded parameters
-- to be wrapped in 'ReqBodyUrlEnc'.

newtype FormUrlEncodedParam = FormUrlEncodedParam [(Text, Maybe Text)]
  deriving (Semigroup, Monoid)

instance QueryParam FormUrlEncodedParam where
  queryParam name mvalue =
    FormUrlEncodedParam [(name, toQueryParam <$> mvalue)]

-- | Multipart form data. Please consult the
-- "Network.HTTP.Client.MultipartFormData" module for how to construct
-- parts, then use 'reqBodyMultipart' to create actual request body from the
-- parts. 'reqBodyMultipart' is the only way to get a value of type
-- 'ReqBodyMultipart', as its constructor is not exported on purpose.
--
-- @since 0.2.0
--
-- ==== __Example__
--
-- > import Control.Exception (throwIO)
-- > import qualified Network.HTTP.Client.MultipartFormData as LM
-- > import Network.HTTP.Req
-- >
-- > instance MonadHttp IO where
-- >   handleHttpException = throwIO
-- >
-- > main :: IO ()
-- > main = do
-- >   body <-
-- >     reqBodyMultipart
-- >       [ LM.partBS "title" "My Image"
-- >       , LM.partFileSource "file1" "/tmp/image.jpg"
-- >       ]
-- >   response <-
-- >     req POST (http "example.com" /: "post")
-- >       body
-- >       bsResponse
-- >       mempty
-- >   print $ responseBody response

data ReqBodyMultipart = ReqBodyMultipart ByteString LI.RequestBody

instance HttpBody ReqBodyMultipart where
  getRequestBody (ReqBodyMultipart _ body) = body
  getRequestContentType (ReqBodyMultipart boundary _) =
    pure ("multipart/form-data; boundary=" <> boundary)

-- | Create 'ReqBodyMultipart' request body from a collection of 'LM.Part's.
--
-- @since 0.2.0

reqBodyMultipart :: MonadIO m => [LM.Part] -> m ReqBodyMultipart
reqBodyMultipart parts = liftIO $ do
  boundary <- LM.webkitBoundary
  body     <- LM.renderParts boundary parts
  return (ReqBodyMultipart boundary body)

-- | A type class for things that can be interpreted as an HTTP
-- 'L.RequestBody'.

class HttpBody body where

  {-# MINIMAL getRequestBody #-}

  -- | How to get actual 'L.RequestBody'.

  getRequestBody :: body -> L.RequestBody

  -- | This method allows to optionally specify value of @Content-Type@
  -- header that should be used with particular body option. By default it
  -- returns 'Nothing' and so @Content-Type@ is not set.

  getRequestContentType :: body -> Maybe ByteString
  getRequestContentType = const Nothing

-- | The type function recognizes 'NoReqBody' as having 'NoBody', while any
-- other body option 'CanHaveBody'. This forces user to use 'NoReqBody' with
-- 'GET' method and other methods that should not send a body.

type family ProvidesBody body :: CanHaveBody where
  ProvidesBody NoReqBody = 'NoBody
  ProvidesBody body      = 'CanHaveBody

-- | This type function allows any HTTP body if method says it
-- 'CanHaveBody'. When the method says it should have 'NoBody', the only
-- body option to use is 'NoReqBody'.
--
-- __Note__: users of GHC 8.0.1 and later will see a slightly more friendly
-- error message when method does not allow a body and body is provided.

type family HttpBodyAllowed
  (allowsBody   :: CanHaveBody)
  (providesBody :: CanHaveBody) :: Constraint where
  HttpBodyAllowed 'NoBody      'NoBody = ()
  HttpBodyAllowed 'CanHaveBody body    = ()
#if MIN_VERSION_base(4,9,0)
  HttpBodyAllowed 'NoBody 'CanHaveBody = TypeError
    ('Text "This HTTP method does not allow attaching a request body.")
#endif

instance HttpBody body => RequestComponent (Womb "body" body) where
  getRequestMod (Womb body) = Endo $ \x ->
    x { L.requestBody = getRequestBody body
      , L.requestHeaders =
        let old = L.requestHeaders x in
          case getRequestContentType body of
            Nothing -> old
            Just contentType ->
              (Y.hContentType, contentType) : old }

----------------------------------------------------------------------------
-- Request—Optional parameters

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

data Option (scheme :: Scheme) =
  Option (Endo (Y.QueryText, L.Request)) (Maybe (L.Request -> IO L.Request))
  -- NOTE 'QueryText' is just [(Text, Maybe Text)], we keep it along with
  -- Request to avoid appending to existing query string in request every
  -- time new parameter is added. Additional Maybe (L.Request -> IO
  -- L.Request) is a finalizer that will be applied after all other
  -- transformations. This is for authentication methods that sign requests
  -- based on data in Request.

instance Semigroup (Option scheme) where
  Option er0 mr0 <> Option er1 mr1 = Option
    (er0 <> er1) (mr0 <|> mr1)

instance Monoid (Option scheme) where
  mempty  = Option mempty Nothing
  mappend = (<>)

-- | A helper to create an 'Option' that modifies only collection of query
-- parameters. This helper is not a part of the public API.

withQueryParams :: (Y.QueryText -> Y.QueryText) -> Option scheme
withQueryParams f = Option (Endo (first f)) Nothing

-- | A helper to create an 'Option' that modifies only 'L.Request'. This
-- helper is not a part of public API.

withRequest :: (L.Request -> L.Request) -> Option scheme
withRequest f = Option (Endo (second f)) Nothing

-- | A helper to create an 'Option' that adds a finalizer (request
-- transformation that is applied after all other modifications).

asFinalizer :: (L.Request -> IO L.Request) -> Option scheme
asFinalizer = Option mempty . pure

instance RequestComponent (Option scheme) where
  getRequestMod (Option f _) = Endo $ \x ->
    let (qparams, x') = appEndo f ([], x)
        query         = Y.renderQuery True (Y.queryTextToQuery qparams)
    in x' { L.queryString = query }

-- | Finalize given 'L.Request' by applying a finalizer from the given
-- 'Option' (if it has any).

finalizeRequest :: MonadIO m => Option scheme -> L.Request -> m L.Request
finalizeRequest (Option _ mfinalizer) = liftIO . fromMaybe pure mfinalizer

----------------------------------------------------------------------------
-- Request—Optional parameters—Query Parameters

-- $query-parameters
--
-- This section describes a polymorphic interface that can be used to
-- construct query parameters (of the type 'Option') and form URL-encoded
-- bodies (of the type 'FormUrlEncodedParam').

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
-- the following URL @a@ is a flag, while @b@ is a query parameter with a
-- value:
--
-- > https://httpbin.org/foo/bar?a&b=10
--
-- This operator is defined in terms of 'queryParam':
--
-- > queryFlag name = queryParam name (Nothing :: Maybe ())

queryFlag :: QueryParam param => Text -> param
queryFlag name = queryParam name (Nothing :: Maybe ())

-- | A type class for query-parameter-like things. The reason to have an
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
-- Request—Optional parameters—Headers

-- | Create an 'Option' that adds a header. Note that if you 'mappend' two
-- headers with the same names the leftmost header will win. This means, in
-- particular, that you cannot create a request with several headers of the
-- same name.

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
-- Request—Optional parameters—Cookies

-- $cookies
--
-- Support for cookies is quite minimalistic at the moment. It's possible to
-- specify which cookies to send using 'cookieJar' and inspect 'L.Response'
-- to extract 'L.CookieJar' from it (see 'responseCookieJar').

-- | Use the given 'L.CookieJar'. A 'L.CookieJar' can be obtained from a
-- 'L.Response' record.

cookieJar :: L.CookieJar -> Option scheme
cookieJar jar = withRequest $ \x ->
  x { L.cookieJar = Just jar }

----------------------------------------------------------------------------
-- Request—Optional parameters—Authentication

-- $authentication
--
-- This section provides the common authentication helpers in the form of
-- 'Option's. You should always prefer the provided authentication 'Option's
-- to manual construction of headers because it ensures that you only use
-- one authentication method at a time (they overwrite each other) and
-- provides additional type safety that prevents leaking of credentials in
-- the cases when authentication relies on HTTPS for encrypting sensitive
-- data.

-- | The 'Option' adds basic authentication.
--
-- See also: <https://en.wikipedia.org/wiki/Basic_access_authentication>.

basicAuth
  :: ByteString        -- ^ Username
  -> ByteString        -- ^ Password
  -> Option 'Https     -- ^ Auth 'Option'
basicAuth = basicAuthUnsafe

-- | An alternative to 'basicAuth' which works for any scheme. Note that
-- using basic access authentication without SSL/TLS is vulnerable to
-- attacks. Use 'basicAuth' instead unless you know what you are doing.
--
-- @since 0.3.1

basicAuthUnsafe
  :: ByteString        -- ^ Username
  -> ByteString        -- ^ Password
  -> Option scheme     -- ^ Auth 'Option'
basicAuthUnsafe username password = asFinalizer
  (pure . L.applyBasicAuth username password)

-- | The 'Option' adds OAuth1 authentication.
--
-- @since 0.2.0

oAuth1
  :: ByteString        -- ^ Consumer token
  -> ByteString        -- ^ Consumer secret
  -> ByteString        -- ^ OAuth token
  -> ByteString        -- ^ OAuth token secret
  -> Option scheme     -- ^ Auth 'Option'
oAuth1 consumerToken consumerSecret token tokenSecret =
  asFinalizer (OAuth.signOAuth app creds)
  where
    app = OAuth.newOAuth
      { OAuth.oauthConsumerKey    = consumerToken
      , OAuth.oauthConsumerSecret = consumerSecret }
    creds = OAuth.newCredential token tokenSecret

-- | The 'Option' adds an OAuth2 bearer token. This is treated by many
-- services as the equivalent of a username and password.
--
-- The 'Option' is defined as:
--
-- > oAuth2Bearer token = header "Authorization" ("Bearer " <> token)
--
-- See also: <https://en.wikipedia.org/wiki/OAuth>.

oAuth2Bearer
  :: ByteString        -- ^ Token
  -> Option 'Https     -- ^ Auth 'Option'
oAuth2Bearer token = asFinalizer
  (pure . attachHeader "Authorization" ("Bearer " <> token))

-- | The 'Option' adds a not-quite-standard OAuth2 bearer token (that seems
-- to be used only by GitHub). This will be treated by whatever services
-- accept it as the equivalent of a username and password.
--
-- The 'Option' is defined as:
--
-- > oAuth2Token token = header "Authorization" ("token" <> token)
--
-- See also: <https://developer.github.com/v3/oauth#3-use-the-access-token-to-access-the-api>.

oAuth2Token
  :: ByteString        -- ^ Token
  -> Option 'Https     -- ^ Auth 'Option'
oAuth2Token token = asFinalizer
  (pure . attachHeader "Authorization" ("token " <> token))

----------------------------------------------------------------------------
-- Request—Optional parameters—Other

-- | Specify the port to connect to explicitly. Normally, 'Url' you use
-- determines the default port: @80@ for HTTP and @443@ for HTTPS. This
-- 'Option' allows to choose arbitrary port overwriting the defaults.

port :: Int -> Option scheme
port n = withRequest $ \x ->
  x { L.port = n }

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

-- | Specify the number of microseconds to wait for response. The default
-- value is 30 seconds.

responseTimeout
  :: Int               -- ^ Number of microseconds to wait
  -> Option scheme
responseTimeout n = withRequest $ \x ->
  x { L.responseTimeout = LI.ResponseTimeoutMicro n }

-- | HTTP version to send to the server, the default is HTTP 1.1.

httpVersion
  :: Int               -- ^ Major version number
  -> Int               -- ^ Minor version number
  -> Option scheme
httpVersion major minor = withRequest $ \x ->
  x { L.requestVersion = Y.HttpVersion major minor }

----------------------------------------------------------------------------
-- Response interpretations

-- | Make a request and ignore the body of the response.

data IgnoreResponse = IgnoreResponse (L.Response ())

instance HttpResponse IgnoreResponse where
  type HttpResponseBody IgnoreResponse = ()
  toVanillaResponse (IgnoreResponse response) = response
  getHttpResponse request manager =
    IgnoreResponse <$> liftIO (L.httpNoBody request manager)
  makeResponseBodyPreview _ = "<ignored response>"

-- | Use this as the fourth argument of 'req' to specify that you want it to
-- ignore the response body.

ignoreResponse :: Proxy IgnoreResponse
ignoreResponse = Proxy

-- | Make a request and interpret the body of the response as JSON. The
-- 'handleHttpException' method of 'MonadHttp' instance corresponding to
-- monad in which you use 'req' will determine what to do in the case when
-- parsing fails (the 'JsonHttpException' constructor will be used).

data JsonResponse a = JsonResponse (L.Response a) ByteString

instance FromJSON a => HttpResponse (JsonResponse a) where
  type HttpResponseBody (JsonResponse a) = a
  toVanillaResponse (JsonResponse response _) = response
  getHttpResponse request manager = do
    response <- L.httpLbs request manager
    case A.eitherDecode (L.responseBody response) of
      Left e -> throwIO (JsonHttpException e)
      Right x -> do
        let preview
              = BL.toStrict
              . BL.take bodyPreviewLength
              . L.responseBody
              $ response
        return $ JsonResponse response { L.responseBody = x } preview
  makeResponseBodyPreview (JsonResponse _ preview) = preview

-- | Use this as the fourth argument of 'req' to specify that you want it to
-- return the 'JsonResponse' interpretation.

jsonResponse :: Proxy (JsonResponse a)
jsonResponse = Proxy

-- | Make a request and interpret the body of the response as a strict
-- 'ByteString'.

newtype BsResponse = BsResponse (L.Response ByteString)

instance HttpResponse BsResponse where
  type HttpResponseBody BsResponse = ByteString
  toVanillaResponse (BsResponse response) = response
  getHttpResponse request manager =
    BsResponse <$> httpBs request manager
  makeResponseBodyPreview =
    B.take bodyPreviewLength . responseBody

-- | Use this as the fourth argument of 'req' to specify that you want to
-- interpret the response body as a strict 'ByteString'.

bsResponse :: Proxy BsResponse
bsResponse = Proxy

-- | Make a request and interpret the body of the response as a lazy
-- 'BL.ByteString'.

newtype LbsResponse = LbsResponse (L.Response BL.ByteString)

instance HttpResponse LbsResponse where
  type HttpResponseBody LbsResponse = BL.ByteString
  toVanillaResponse (LbsResponse response) = response
  getHttpResponse request manager =
    LbsResponse <$> L.httpLbs request manager
  makeResponseBodyPreview =
    BL.toStrict . BL.take bodyPreviewLength . responseBody

-- | Use this as the fourth argument of 'req' to specify that you want to
-- interpret the response body as a lazy 'BL.ByteString'.

lbsResponse :: Proxy LbsResponse
lbsResponse = Proxy

-- | Perform a 'L.Request' using given 'L.Manager' and return the response
-- as a strict 'ByteString'.

httpBs :: L.Request -> L.Manager -> IO (L.Response ByteString)
httpBs request manager = L.withResponse request manager $ \response -> do
  chunks <- L.brConsume (L.responseBody response)
  return response { L.responseBody = B.concat chunks }

----------------------------------------------------------------------------
-- Inspecting a response

-- | Get the response body.

responseBody
  :: HttpResponse response
  => response
  -> HttpResponseBody response
responseBody = L.responseBody . toVanillaResponse

-- | Get the response status code.

responseStatusCode
  :: HttpResponse response
  => response
  -> Int
responseStatusCode =
  Y.statusCode . L.responseStatus . toVanillaResponse

-- | Get the response status message.

responseStatusMessage
  :: HttpResponse response
  => response
  -> ByteString
responseStatusMessage =
  Y.statusMessage . L.responseStatus . toVanillaResponse

-- | Lookup a particular header from a response.

responseHeader
  :: HttpResponse response
  => response          -- ^ Response interpretation
  -> ByteString        -- ^ Header to lookup
  -> Maybe ByteString  -- ^ Header value if found
responseHeader r h =
  (lookup (CI.mk h) . L.responseHeaders . toVanillaResponse) r

-- | Get the response 'L.CookieJar'.

responseCookieJar
  :: HttpResponse response
  => response
  -> L.CookieJar
responseCookieJar = L.responseCookieJar . toVanillaResponse

----------------------------------------------------------------------------
-- Response—Defining your own interpretation

-- $new-response-interpretation
--
-- To create a new response interpretation you just need to make your data
-- type an instance of 'HttpResponse' type class.

-- | A type class for response interpretations. It allows to fully control
-- how request is made and how its body is parsed.

class HttpResponse response where

  -- | The associated type is the type of body that can be extracted from an
  -- instance of 'HttpResponse'.

  type HttpResponseBody response :: *

  -- | The method describes how to get the underlying 'L.Response' record.

  toVanillaResponse :: response -> L.Response (HttpResponseBody response)

  -- | This method describes how to make an HTTP request given 'L.Request'
  -- (prepared by the library) and 'L.Manager'.

  getHttpResponse :: L.Request -> L.Manager -> IO response

  -- | Construct a “preview” of response body. It is recommend to limit the
  -- length to 1024 bytes. This is mainly useful for inclusion of response
  -- body fragments in exceptions.
  --
  -- __Note__: in versions 0.3.0–0.4.0 this function returned @'IO'
  -- 'ByteString'@.
  --
  -- @since 0.5.0

  makeResponseBodyPreview :: response -> ByteString

----------------------------------------------------------------------------
-- Other

-- | The main class for things that are “parts” of 'L.Request' in the sense
-- that if we have a 'L.Request', then we know how to apply an instance of
-- 'RequestComponent' changing\/overwriting something in it. 'Endo' is a
-- monoid of endomorphisms under composition, it's used to chain different
-- request components easier using @('<>')@.

class RequestComponent a where

  -- | Get a function that takes a 'L.Request' and changes it somehow
  -- returning another 'L.Request'. For example HTTP method instance of
  -- 'RequestComponent' just overwrites method. The function is wrapped in
  -- 'Endo' so it's easier to chain such “modifying applications” together
  -- building bigger and bigger 'RequestComponent's.

  getRequestMod :: a -> Endo L.Request

-- | This wrapper is only used to attach a type-level tag to a given type.
-- This is necessary to define instances of 'RequestComponent' for any thing
-- that implements 'HttpMethod' or 'HttpBody'. Without the tag, GHC can't
-- see the difference between @'HttpMethod' method => 'RequestComponent'
-- method@ and @'HttpBody' body => 'RequestComponent' body@ when it decides
-- which instance to use (i.e. the constraints are taken into account later,
-- when instance is already chosen).

newtype Womb (tag :: Symbol) a = Womb a

-- | Exceptions that this library throws.

data HttpException
  = VanillaHttpException L.HttpException
    -- ^ A wrapper with an 'L.HttpException' from "Network.HTTP.Client"
  | JsonHttpException String
    -- ^ A wrapper with Aeson-produced 'String' describing why decoding
    -- failed
  deriving (Show, Typeable, Generic)

instance Exception HttpException

-- | A simple 'Bool'-like type we only have for better error messages. We
-- use it as a kind and its data constructors as type-level tags.
--
-- See also: 'HttpMethod' and 'HttpBody'.

data CanHaveBody
  = CanHaveBody        -- ^ Indeed can have a body
  | NoBody             -- ^ Should not have a body

-- | A type-level tag that specifies URL scheme used (and thus if HTTPS is
-- enabled). This is used to force TLS requirement for some authentication
-- 'Option's.

data Scheme
  = Http               -- ^ HTTP
  | Https              -- ^ HTTPS
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

----------------------------------------------------------------------------
-- Constants

-- | Max length of preview fragment of response body.

bodyPreviewLength :: Num a => a
bodyPreviewLength = 1024
