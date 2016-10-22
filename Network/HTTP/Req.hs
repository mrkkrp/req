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
-- The documentation below is structured in such a way that most important
-- information goes first: you learn how to do HTTP requests, then how to
-- embed them any monad you have, then it goes on giving you details about
-- less-common things you may want to know about. The documentation is
-- written with sufficient coverage of details and with examples, it's
-- designed to be a complete tutorial on its own.
--
-- The library uses the following well-known and mature packages under the
-- hood to guarantee you best experience without bugs or other funny
-- business:
--
--     * <https://hackage.haskell.org/package/http-client> — low level HTTP
--       client used everywhere in Haskell.
--     * <https://hackage.haskell.org/package/http-client-tls> — TLS (HTTPS)
--       support.
--     * <https://hackage.haskell.org/package/http-conduit> — conduit
--       interface to @http-client@.
--
-- You generally won't need low-level interface of @http-client@ at all
-- because this package covers /everything/, so don't import it or import
-- qualified because it has naming conflicts with @req@.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Network.HTTP.Req
  ( -- * Making a request
    req
    -- * Embedding requests into your monad
  , MonadHttp  (..)
  , HttpConfig (..)
  , httpConfig
  , httpsConfig
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
  , http
  , https
  , (/:)
  , Endpoint
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
    -- *** Authorization
    -- *** Other
  , port
  , redirectCount
    -- * Response
  , HttpResponse (..)
    -- * Other
  , CanHaveBody (..)
  , Scheme (..) )
where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString
import Data.Proxy
import Data.Semigroup hiding (Option)
import Data.Text (Text)
import GHC.TypeLits
import Numeric.Natural
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Network.HTTP.Client  as L
import qualified Network.HTTP.Types   as Y

----------------------------------------------------------------------------
-- Making a request

-- | Perform an HTTP request.

req
  :: ( MonadHttp          m
     , HttpMethod         method
     , HttpBody           body
     , HttpResponse       response
     , AllowsBody         method ~ ProvidesBody body
     , ProvidesConnection m ~ scheme )
  => method
  -> Endpoint scheme
  -> body
  -> Option
  -> m response
req = undefined

-- TODO We need to catch exceptions here, only relevant ones and only
-- synchronous to give user a chance to work with pure, explicit exceptions
-- if he wants to do so.

-- TODO How to specify return type here?
-- TODO It needs a way to signal errors.

----------------------------------------------------------------------------
-- Embedding requests into your monad

class MonadIO m => MonadHttp m where
  type ProvidesConnection m :: Scheme
  getHttpConfig :: m (HttpConfig (ProvidesConnection m))

-- | 'HttpConfig' contains general and default settings to be used when
-- making HTTP requests.

data HttpConfig (scheme :: Scheme) = HttpConfig
  { httpConfigManager :: L.Manager
  }

httpConfig :: MonadIO m => m (HttpConfig 'Http)
httpConfig = undefined -- FIXME not final, need to pass params in

httpsConfig :: MonadIO m => m (HttpConfig 'Https)
httpsConfig = undefined -- FIXME

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
-- >   type AllowsMody COPY = 'CanHaveBody
-- >   httpMethodName Proxy = "COPY"

class HttpMethod a where

  -- | Type function 'AllowsBody' returns type of kind 'CanHaveBody' which
  -- tells the rest of the library whether the method can have a body or
  -- not. We use the special type 'CanHaveBody' “lifted” into kind instead
  -- of 'Bool' to get more user-friendly compiler messages.

  type AllowsBody a :: CanHaveBody

  -- | Return name of the method as a 'ByteString'.

  httpMethodName :: Proxy a -> Y.Method

-- NOTE Now we state how to get an endomorphism on 'Request's that changes
-- given 'Request' so it has the specified method.

instance HttpMethod method => RequestComponent (Womb "method" method) where
  getRequestMod _ = Endo $ \x ->
    x { L.method = httpMethodName (Proxy :: Proxy method) }

----------------------------------------------------------------------------
-- Request — URL

newtype Endpoint (scheme :: Scheme) = Endpoint { unEndpoint :: Text }

-- data Endpoint (s :: Scheme) where
--   Http'  :: Text -> Endpoint 'Http
--   Https' :: Text -> Endpoint 'Https
--   (:/) :: Endpoint s -> Text -> Endpoint s

http :: Text -> Endpoint 'Http
http = Endpoint . percentEncode

https :: Text -> Endpoint 'Https
https = Endpoint . percentEncode

infixr 5 /:

(/:) :: Endpoint scheme -> Text -> Endpoint scheme
(/:) = undefined -- TODO

-- TODO Escape things in endpoint and query parameters, only :/ should work
-- as real /

parseUrlHttp :: Text -> Endpoint 'Http
parseUrlHttp = undefined

parseUrlHttps :: Text -> Endpoint 'Https
parseUrlHttps = undefined

----------------------------------------------------------------------------
-- Request body

class HttpBody b where
  type ProvidesBody b :: CanHaveBody
  getReqestBody :: b -> ByteString -- FIXME should use a conduit here

data UrlEncodedParam = UrlEncodedParam Text (Maybe Text)

instance QueryParam UrlEncodedParam where
  queryParam = UrlEncodedParam

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
-- Request — Optional parameters — Authorization

-- Hmm, need to take a look at wreq first…

----------------------------------------------------------------------------
-- Request — Optional parameters — Other

port :: Natural -> Option
port = undefined -- TODO

redirectCount :: Natural -> Option
redirectCount = undefined -- TODO

-- etc.

----------------------------------------------------------------------------
-- Response

-- Here we need to provide various options how to consume responses.

class HttpResponse response where
  interpretResponse :: ByteString -> response -- FIXME use conduit

----------------------------------------------------------------------------
-- Other

newtype Womb (tag :: Symbol) a = Womb { unWomb :: a }

class RequestComponent a where
  getRequestMod :: a -> Endo L.Request

data CanHaveBody = CanHaveBody | NoBody

data Scheme = Http | Https

percentEncode :: Text -> Text
percentEncode = undefined
