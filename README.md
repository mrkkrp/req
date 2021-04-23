# Req

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/req.svg?style=flat)](https://hackage.haskell.org/package/req)
[![Stackage Nightly](http://stackage.org/package/req/badge/nightly)](http://stackage.org/nightly/package/req)
[![Stackage LTS](http://stackage.org/package/req/badge/lts)](http://stackage.org/lts/package/req)
![CI](https://github.com/mrkkrp/req/workflows/CI/badge.svg?branch=master)

* [Motivation and Req vs other libraries](#motivation-and-req-vs-other-libraries)
* [Unsolved problems](#unsolved-problems)
* [Related packages](#related-packages)
* [Blog posts](#blog-posts)
* [Contribution](#contribution)
* [License](#license)

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class
import Data.Aeson
import Network.HTTP.Req

main :: IO ()
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
main = runReq defaultHttpConfig $ do
  let payload =
        object
          [ "foo" .= (10 :: Int),
            "bar" .= (20 :: Int)
          ]
  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <-
    req
      POST -- method
      (https "httpbin.org" /: "post") -- safe by construction URL
      (ReqBodyJson payload) -- use built-in options or add your own
      jsonResponse -- specify how to interpret response
      mempty -- query params, headers, explicit port number, etc.
  liftIO $ print (responseBody r :: Value)
```

Req is an HTTP client library that attempts to be easy-to-use, type-safe,
and expandable.

“Easy-to-use” means that the library is designed to be beginner-friendly so
it's simple to add to your monad stack, intuitive to work with,
well-documented, and does not get in your way. Doing HTTP requests is a
common task and a Haskell library for this should be approachable and clear
to beginners, thus certain compromises were made. For example, one cannot
currently modify `ManagerSettings` of the default manager because the
library always uses the same implicit global manager for simplicity and
maximal connection sharing. There is a way to use your own manager with
different settings, but it requires more typing.

“Type-safe” means that the library tries to eliminate certain classes of
errors. For example, we have correct-by-construction URLs; it is guaranteed
that the user does not send the request body when using methods like GET or
OPTIONS, and the amount of implicit assumptions is minimized by making the
user specify their intentions in an explicit form. For example, it's not
possible to avoid specifying the body or the method of a request.
Authentication methods that assume HTTPS force the user to use HTTPS at the
type level.

“Expandable” refers to the ability to create new components without having
to resort to hacking. For example, it's possible to define your own HTTP
methods, create new ways to construct the body of a request, create new
authorization options, perform a request in a different way, and create your
own methods to parse a response.

The library uses the following mature packages under the hood to guarantee
you the best experience:

* [`http-client`](https://hackage.haskell.org/package/http-client)—low level
  HTTP client used everywhere in Haskell.
* [`http-client-tls`](https://hackage.haskell.org/package/http-client-tls)—TLS
  (HTTPS) support for `http-client`.

It is important to note that since we leverage well-known libraries that the
whole Haskell ecosystem uses, there is no risk in using Req. The machinery
for performing requests is the same as with `http-conduit` and Wreq. The
only difference is the API.

## Related packages

The following packages are designed to be used with Req:

* [`req-conduit`](https://hackage.haskell.org/package/req-conduit)—support
  for streaming request and response bodies in constant memory.

If you happen to have written a package that adds new features to Req,
please submit a PR to include it in this list.

## Blog posts

* [Req 1.0.0, HTTP client, and streaming](https://markkarpov.com/post/req-1.0.0-http-client-and-streaming.html)

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/req/issues).

Pull requests are also welcome.

## License

Copyright © 2016–present Mark Karpov

Distributed under BSD 3 clause license.
