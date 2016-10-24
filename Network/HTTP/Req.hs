-- |
-- Module      :  Network.HTTP.Req
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This is an easy-to-use, type-safe, expandable, high-level HTTP library
-- that just works without any fooling around.
--
-- /(A modest intro goes here, click on 'req' to start making requests.)/
--
-- What does the “easy-to-use” phrase mean? It means that the library is
-- designed to be beginner-friendly, so it's simple to add it to your monad
-- stack, intuitive to work with, well-documented, and does not get in your
-- way. On this path certain compromises were made. For example one cannot
-- currently modify 'L.ManagerSettings' of default manager because the
-- library always use the same implicit global manager for simplicity and
-- maximal connection sharing. There is a way to use your own manager with
-- different settings, but it requires a bit more typing. Doing HTTP
-- requests is a common task and Haskell library for this should be very
-- approachable and clear to beginners.
--
-- “Type-safe” means that the library is protective and eliminates certain
-- class of errors compared to alternative libraries like @wreq@ or vanilla
-- @http-client@. For example we have correct-by-construction 'Url's, it's
-- guaranteed that user does not send request body when using methods like
-- 'GET' or 'DELETE', amount of implicit assumptions is minimized by making
-- user specify his\/her intentions in explicit form (for example it's not
-- possible to avoid specifying body or method of a request). It carefully
-- hides underlying types from lower-level @http-client@ package because
-- it's not type safe enough (for example 'L.Request' is an instance of
-- 'Data.String.IsString' and if it's malformed, it will blow up at
-- run-time).
--
-- “Expandable” refers to the ability of the library to be expanded without
-- ugly hacking. For example it's possible to define your own HTTP methods,
-- new ways to construct body of request, new authorization options, new
-- ways to actually perform request and how to represent\/parse its
-- response. As user extends the library to satisfy his\/her special needs,
-- the new solutions work just like built-ins. That said, all common cases
-- are covered by the library out-of-the-box.
--
-- “High-level” means that there are less details to worry about. The
-- library is a result of my experiences as a Haskell consultant, working
-- for several clients who have very different projects and so the library
-- adapts easily to any particular style of writing Haskell applications.
-- For example some people prefer throwing exceptions, while others are
-- concerned with purity: just define 'handleHttpException' accordingly when
-- making your monad instance of 'MonadHttp' and it will play seamlessly.
-- Finally, the library cuts boilerplate considerably and helps write
-- concise, easy to read code, thanks to its minimal and uniform API.
--
-- The documentation below is structured in such a way that most important
-- information goes first: you learn how to do HTTP requests, then how to
-- embed them in any monad you have, then it goes on giving you details
-- about less-common things you may want to know about. The documentation is
-- written with sufficient coverage of details and examples, it's designed
-- to be a complete tutorial on its own.
--
-- The library uses the following well-known and mature packages under the
-- hood to guarantee you best experience without bugs or other funny
-- business:
--
--     * <https://hackage.haskell.org/package/http-client> — low level HTTP
--       client used everywhere in Haskell.
--     * <https://hackage.haskell.org/package/http-client-tls> — TLS (HTTPS)
--       support for @http-client@.
--     * <https://hackage.haskell.org/package/http-conduit> — conduit
--       interface to @http-client@.
--
-- You won't need low-level interface of @http-client@ most of the time, but
-- when you do, it's better import it qualified because it has naming
-- conflicts with @req@.

{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE DeriveDataTypeable                 #-}
{-# LANGUAGE DeriveGeneric                      #-}
{-# LANGUAGE FlexibleInstances                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving         #-}
{-# LANGUAGE KindSignatures                     #-}
{-# LANGUAGE RecordWildCards                    #-}
{-# LANGUAGE ScopedTypeVariables                #-}
{-# LANGUAGE TypeFamilies                       #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Network.HTTP.Req
  ( -- * Making a request
    -- $making-a-request
    req
    -- * Embedding requests into your monad
    -- $embedding-requests
  , MonadHttp  (..)
  , HttpConfig (..)
    -- * Request
    -- ** Methods
    -- $request-methods
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
  , Url
  , http
  , https
  , (/:)
    -- ** Body
  , HttpBody (..) -- TODO more stuff here
    -- ** Optional parameters
  , Option
    -- *** Query parameters
  , (=:)
  , queryFlag
  , QueryParam (..)
    -- *** Headers
  , header
    -- *** Cookies
    -- *** Authorization
    -- *** Other
  , port
    -- * Response
  , HttpResponse (..)
    -- * Other
  , CanHaveBody (..) )
where

import Control.Exception (try)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString
import Data.Data (Data)
import Data.Default.Class
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Semigroup hiding (Option)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Binary.Builder          as R
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import qualified Data.List.NonEmpty           as NE
import qualified Data.Text.Encoding           as T
import qualified Network.Connection           as NC
import qualified Network.HTTP.Client          as L
import qualified Network.HTTP.Client.Internal as LI
import qualified Network.HTTP.Client.TLS      as L
import qualified Network.HTTP.Types           as Y

----------------------------------------------------------------------------
-- Making a request

-- $making-a-request
--
-- To make an HTTP request you need only one function: 'req'.

-- | Make an HTTP request.
--
-- TODO Finish docs of this function when the package is more developed.

req
  :: forall m method body response.
     ( MonadHttp    m
     , HttpMethod   method
     , HttpBody     body
     , HttpResponse response
     , AllowsBody   method ~ ProvidesBody body )
  => method            -- ^ HTTP method
  -> Url               -- ^ 'Url' — location of resource
  -> body              -- ^ Body of the request
  -> Option            -- ^ Collection of optional parameters
  -> m response        -- ^ Response
req method url body options = do
  config  <- getHttpConfig
  manager <- liftIO (readIORef globalManager)
  let request = flip appEndo L.defaultRequest $
        -- NOTE Order of 'mappend's matters, here method is overwritten
        -- first and 'config' takes its effect last. In particular, this
        -- means that 'options' can overwrite things set by 'url' and
        -- 'body', which is useful for setting port number, "Content-Type"
        -- header, etc.
        getRequestMod config                                <>
        getRequestMod options                               <>
        getRequestMod (Womb body   :: Womb "body"   body)   <>
        getRequestMod url                                   <>
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
-- instance of the 'MonadHttp' type class, which see.

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
  { httpConfigProxy :: Maybe L.Proxy
    -- ^ Proxy to use. By default values of @HTTP_PROXY@ and @HTTPS_PROXY@
    -- environment variables are respected, this setting overwrites them.
    -- Default value: 'Nothing'.
  , httpConfigRedirectCount :: Word
    -- ^ How many redirects to follow when getting a resource. Default
    -- value: 10.
  , httpConfigAltManager :: Maybe L.Manager
    -- ^ Alternative 'L.Manager' to use. 'Nothing' (default value) means
    -- that default implicit manager will be used (that's what you want in
    -- 99% of cases).
  } deriving Typeable

instance Default HttpConfig where
  def = HttpConfig
    { httpConfigProxy         = Nothing
    , httpConfigRedirectCount = 10
    , httpConfigAltManager    = Nothing }

instance RequestComponent HttpConfig where
  getRequestMod HttpConfig {..} = Endo $ \x ->
    x { L.proxy                   = httpConfigProxy
      , L.redirectCount           = fromIntegral httpConfigRedirectCount
      , LI.requestManagerOverride = httpConfigAltManager }

----------------------------------------------------------------------------
-- Request — Methods

-- $request-methods
--
-- The package provides all methods as defined by RFC 2616, and 'PATCH'
-- which is defined by RFC 5789 — that should be enough to talk to RESTful
-- APIs. In some cases however, you may want to add more methods (e.g. you
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

-- | 'DELETE' method.

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

  httpMethodName :: Proxy a -> Y.Method

instance HttpMethod method => RequestComponent (Womb "method" method) where
  getRequestMod _ = Endo $ \x ->
    x { L.method = httpMethodName (Proxy :: Proxy method) }

----------------------------------------------------------------------------
-- Request — URL

-- | Request's 'Url'. Start constructing your 'Url' with 'http' or 'https'
-- specifying the scheme and host at the same time. Then use the @('/:')@
-- constructor to grow path one piece at a time. Every single piece of path
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
-- > https "юникод.рф"
-- > -- https://%D1%8E%D0%BD%D0%B8%D0%BA%D0%BE%D0%B4.%D1%80%D1%84

data Url = Url Bool (NonEmpty Text)
  -- NOTE The first 'Bool' value specifies if the 'Url' has “https” as its
  -- scheme (otherwise “http” is assumed). The second value is path segments
  -- in reversed order.
  deriving (Eq, Ord, Data, Typeable, Generic)

-- | Given host name, produce a 'Url' which have “http” as its scheme and
-- empty path. This also sets port to @80@.

http :: Text -> Url
http = Url False . pure

-- | Given host name, produce a 'Url' which have “https” as its scheme and
-- empty path. This also sets port to @443@.

https :: Text -> Url
https = Url True . pure

-- | Grow given 'Url' appending a single path segment to it.

infixl 5 /:
(/:) :: Url -> Text -> Url
Url secure path /: segment = Url secure (NE.cons segment path)

instance RequestComponent Url where
  getRequestMod (Url secure segments) = Endo $ \x ->
    let (host :| path) = NE.reverse segments in
    x { L.secure = secure
      , L.port   = if secure then 443 else 80
      , L.host   = Y.urlEncode False (T.encodeUtf8 host)
      , L.path   =
          (BL.toStrict . R.toLazyByteString . Y.encodePathSegments) path }

----------------------------------------------------------------------------
-- Request — Body

class HttpBody b where
  type ProvidesBody b :: CanHaveBody
  getReqestBody :: b -> ByteString -- FIXME should use a conduit here

data FormUrlEncodedParam = FormUrlEncodedParam Text (Maybe Text)

instance QueryParam FormUrlEncodedParam where
  queryParam = FormUrlEncodedParam

instance HttpBody b => RequestComponent (Womb "body" b) where
  getRequestMod = undefined -- FIXME

-- data ReqBody (b :: HasBody) where
--   NoReqBody  ::                  ReqBody 'NoBody
--   ReqBodyLBS :: BL.ByteString -> ReqBody 'HasBody
--   ReqBodyBS  :: B.ByteString  -> ReqBody 'HasBody
--   ReqBodyBuilder :: Builder   -> ReqBody 'HasBody
--   ReqBodyUrlEncoded :: [(Text,Text)] -> ReqBody 'HasBody
--   --  ↑ use something nicer than tuples? (:=) pairs perhaps? The pairs
--   --  should represent query parameters and body of url encoded stuff at the
--   --  same time.
--   ReqBodyJSON :: Value -> ReqBody 'HasBody
--   -- TODO add more? add conduit support and depend on http-conduit

----------------------------------------------------------------------------
-- Request — Optional parameters

-- | Opaque 'Option' type is a 'Monoid' you can use to pack collection of
-- optional parameters like query parameters and headers. We also provide
-- authorization helpers out-of-the-box which are also of this type.

newtype Option = Option { unOption :: Endo L.Request }
  deriving (Semigroup, Monoid)

instance RequestComponent Option where
  getRequestMod = unOption

----------------------------------------------------------------------------
-- Request — Optional parameters — Query Parameters

-- | Synonym of '(=:)' in form of function, not operator.

infix 7 =:
(=:) :: QueryParam a => Text -> Text -> a
name =: value = queryParam name (pure value)

-- |

queryFlag :: QueryParam a => Text -> a
queryFlag name = queryParam name Nothing

class QueryParam a where
  queryParam :: Text -> Maybe Text -> a

instance QueryParam Option where
  queryParam = undefined -- TODO

----------------------------------------------------------------------------
-- Request — Optional parameters — Headers

header :: Text -> Text -> Option
header = undefined

----------------------------------------------------------------------------
-- Request — Optional parameters — Cookies

-- TODO No idea right now.

----------------------------------------------------------------------------
-- Request — Optional parameters — Authorization

-- Hmm, need to take a look at wreq first…

----------------------------------------------------------------------------
-- Request — Optional parameters — Other

port :: Word -> Option
port = undefined -- TODO

-- redirectCount :: Word -> Option
-- redirectCount = undefined -- TODO

-- etc.

----------------------------------------------------------------------------
-- Response

-- Here we need to provide various options how to consume responses.

class HttpResponse response where
  getHttpResponse :: L.Manager -> L.Request -> IO response

-- helpers to

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
