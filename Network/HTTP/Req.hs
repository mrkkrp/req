-- |
-- Module      :  Network.HTTP.Req
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Library for safe construction of HTTP requests.

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.HTTP.Req
  ( HasBody (..)
  , HttpMethod (..)
  , Scheme (..)
  , Endpoint (..)
  , ReqBody (..)
  , req )
where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Binary.Builder (Builder)
import Data.Semigroup hiding (Option)
import Data.Text (Text)
import Network.HTTP.Client
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

data HasBody = HasBody | NoBody

data HttpMethod (b :: HasBody) where
  GET     :: HttpMethod 'NoBody
  HEAD    :: HttpMethod 'NoBody
  OPTIONS :: HttpMethod 'NoBody
  DELETE  :: HttpMethod 'NoBody
  POST    :: HttpMethod 'HasBody
  PUT     :: HttpMethod 'HasBody
  PATCH   :: HttpMethod 'HasBody

data Scheme = Http | Https

data Endpoint (s :: Scheme) where
  Http'  :: Text -> Endpoint 'Http
  Https' :: Text -> Endpoint 'Https
  (:/) :: Endpoint s -> Text -> Endpoint s

http = Http' -- don't export the constructors

https = Https'

infixl 5 :/

data ReqBody (b :: HasBody) where
  NoReqBody  ::                  ReqBody 'NoBody
  ReqBodyLBS :: BL.ByteString -> ReqBody 'HasBody
  ReqBodyBS  :: B.ByteString  -> ReqBody 'HasBody
  ReqBodyBuilder :: Builder   -> ReqBody 'HasBody
  ReqBodyUrlEncoded :: [(Text,Text)] -> ReqBody 'HasBody
  --  ↑ use something nicer than tuples? (:=) pairs perhaps?
  ReqBodyJSON :: Value -> ReqBody 'HasBody
  -- TODO add more?

newtype Options = Options [Option] deriving (Semigroup, Monoid)

data Option
  = QP Text Text  -- ^ Query param
  -- TODO add much more, headers, cookies, all the optional stuff

data ReqConfig (s :: Scheme) = ReqConfig Manager

class HasReqConfig a s | a -> s where
  reqConfig :: a -> ReqConfig s

instance HasReqConfig (ReqConfig s) s where
  reqConfig = id

req
  :: (MonadIO m, MonadReader r m, HasReqConfig r s)
  => HttpMethod b
  -> Endpoint s
  -> ReqBody b
  -> Options
  -> m ()
req = undefined

-- | Example of request.

myReq :: ReaderT (ReqConfig 'Http) IO ()
myReq = req GET (http "example.org" :/ "v1" :/ "items") NoReqBody mempty
