-- |
-- Module      :  Network.HTTP.Req.OAuth1
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of OAuth 1.0 signing.

module Network.HTTP.Req.OAuth1
  ( signRequest )
where

import Data.ByteString (ByteString)
import Network.HTTP.Client

-- | Sign a given request using OAuth 1.0 signing algorithm.

signRequest
  :: ByteString        -- ^ Consumer token
  -> ByteString        -- ^ Consumer secret
  -> ByteString        -- ^ OAuth token
  -> ByteString        -- ^ OAuth token secret
  -> Request           -- ^ 'Request' to sign
  -> Request           -- ^ Resulting request
signRequest = undefined -- TODO
