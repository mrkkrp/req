-- |
-- Module      :  Network.HTTP.Req.AWS
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Implementation of AWS v4 request signature.

module Network.HTTP.Req.AWS
  ( signRequest )
where

import Data.ByteString (ByteString)
import Network.HTTP.Client

-- | Sign a given request following AWS v4 request signing specification:
--
-- See also: <https://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html>

signRequest
  :: ByteString        -- ^ AWS access key
  -> ByteString        -- ^ AWS secret access key
  -> Request           -- ^ 'Request' to sign
  -> Request           -- ^ Resulting request
signRequest = undefined -- TODO
