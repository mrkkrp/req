# Req

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/req.svg?style=flat)](https://hackage.haskell.org/package/req)
[![Stackage Nightly](http://stackage.org/package/req/badge/nightly)](http://stackage.org/nightly/package/req)
[![Stackage LTS](http://stackage.org/package/req/badge/lts)](http://stackage.org/lts/package/req)
[![Build Status](https://travis-ci.org/mrkkrp/req.svg?branch=master)](https://travis-ci.org/mrkkrp/req)

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

Req is an easy-to-use, type-safe, expandable, high-level HTTP client library
that just works without any fooling around.

What does the phrase “easy-to-use” mean? It means that the library is
designed to be beginner-friendly so it's simple to add to your monad stack,
intuitive to work with, well-documented, and does not get in your way. Doing
HTTP requests is a common task and Haskell library for this should be very
approachable and clear to beginners, thus certain compromises were made. For
example, one cannot currently modify `ManagerSettings` of the default
manager because the library always uses the same implicit global manager for
simplicity and maximal connection sharing. There is a way to use your own
manager with different settings, but it requires a bit more typing.

“Type-safe” means that the library is protective and eliminates certain
classes of errors. For example, we have correct-by-construction URLs, it's
guaranteed that the user does not send the request body when using methods
like GET or OPTIONS, and the amount of implicit assumptions is minimized by
making the user specify his/her intentions in an explicit form (for example,
it's not possible to avoid specifying the body or method of request).
Authentication methods that assume HTTPS force the user to use HTTPS at the
type level. The library also carefully hides underlying types from the
lower-level `http-client` package because those types are not safe enough
(for example `Request` is an instance of `IsString` and, if it's malformed,
it will blow up at run-time).

“Expandable” refers to the ability to create new components for dealing with
HTTP without having to resort to ugly hacking. For example, it's possible to
define your own HTTP methods, create new ways to construct the body of a
request, create new authorization options, perform a request in a different
way, and create your own methods to parse and represent a response. As the
user extends the library to satisfy his/her special needs, the new solutions
will work just like the built-ins. However, all of the common cases are also
covered by the library out-of-the-box.

“High-level” means that there are less details to worry about. The library
is a result of my experiences as a Haskell consultant. Working for several
clients, who had very different projects, showed me that the library should
adapt easily to any particular style of writing Haskell applications. For
example, some people prefer throwing exceptions, while others are concerned
with purity. Just define `handleHttpException` accordingly when making your
monad instance of `MonadHttp` and it will play together seamlessly. Finally,
the library cuts down boilerplate considerably, and helps you write concise,
easy to read, and maintainable code.

The library uses the following mature packages under the hood to guarantee
you the best experience:

* [`http-client`](https://hackage.haskell.org/package/http-client)—low level
  HTTP client used everywhere in Haskell.
* [`http-client-tls`](https://hackage.haskell.org/package/http-client-tls)—TLS
  (HTTPS) support for `http-client`.

It's important to note that since we leverage well-known libraries that the
whole Haskell ecosystem uses, there is no risk in using Req. The machinery
for performing requests is the same as with `http-conduit` and Wreq. The
only difference is the API.

## Motivation and Req vs other libraries

*This section is my opinion and it contains criticisms of other well-known
libraries. If you're a user/fan of one of these libraries, please remember
not to react aggressively and respect the fact that I may have different
views on API design from yours.*

I have spent time to write the library because sending HTTP requests is a
common need, but there is no high-level library for that in Haskell that I
could use with pleasure. I'll explain why.

First of all, there is `http-client` and `http-client-tls`. They just work.
I have no issues with the libraries except that they are too low-level for
my taste. Indeed, even the docs say that they are low-level and “intended as
a base layer for more user-friendly packages”. This is exactly how I use
them in Req, as base level. Req is nothing but a different API to
`http-client`, so it only works because of the hard work put into
`http-client`.

`http-conduit` definitely has its place. For one thing it allows you to
stream request and response bodies in constant memory, what other library
allows you to do that? On the other hand if you take a look at
`Network.HTTP.Simple`, then although it's said that it's a “higher level
API”, it's mostly the same as vanilla `http-client` in spirit/approach and
just adds `conduit`-powered functions to perform requests and allows to use
global implicit `Manager` (Req does the same). If I tried to frame what
exactly I don't like about `http-conduit` in words, then it would be “the
way requests are constructed”. You *set* parameters instead of *being
forced* to declare necessary bits and *being allowed* to declare optional
bits in a way that their combination is valid. Also, with `http-conduit` you
parse request from a string without the protection of TH that otherwise
saves the day as in Yesod.

Then there is Wreq. `wreq` [doesn't see much development
lately](https://github.com/bos/wreq/issues/93). `wreq` is by itself a weird
library, IMO. You have functions per method—not very good, as there may be
new methods, like PATCH which is not new but still missing (well, you have
`customMethod`, but what is the point of having per-method functions if you
have a more general way to use any method? you should be able to just insert
methods in the “argument slot” of `customMethod` and end up with a more
general solution). Now, every method function has a companion that takes
`Options` (like you have `get` and `getWith`). Why the duplication? Where is
generality and flexibility? This is not all though, because you cannot
really use `get` you see in the main module, because you want to have
connection sharing. Wreq's author does not take the gift of automatic
connection re-use `Manager` from `http-client` provides, he invents the
whole new thing of “sessions”. Only inside a session your connections will
be shared and re-used. However with the session stuff you have yet another
set of per-method functions like `get` and `getWith`—these are different
ones, to be used with sessions! Now if you have a multi-threaded app, here
is a surprise for you: you can't share connections between threads as
connections are shared only inside of the `withSession` friend and “session
will no longer be valid after that function returns”. There are valid uses
for sessions, but the point is that they are just too inconvenient for
common tasks.

I used `servant-client` a couple of times but the amount of boilerplate it
requires is frightening. If you have several query parameters, and you use
just one of them, you'll have to pass lots of `Nothing`s.

## Unsolved problems

AWS request signing is problematic because request body can be in the form
of an action to execute (and all that “popper” stuff for streaming), not
just a `ByteString` and so getting its digest (hash) is not trivial without
running the action and consuming body in its entirety before the request in
made. In Wreq the author chose to just use `error` when body is not a
(strict or lazy) `ByteString`. Maybe it's OK for Wreq, but I don't consider
this a proper solution for Req as we support full variety of body options.
For example, what if I want to upload 1 Gb file to S3? I want to stream it
in constant memory but at the same time I need to calculate its hash before
I start streaming. One solution to the problem seems to be in taking the
hash explicitly (as an argument of the hypothetical `awsAuth`) and making it
a responsibility of the user to calculate the hash correctly. I don't like
this because it's not user-friendly. So the question stays open, for now
there is no AWS signing functionality provided out-of-the-box. The best
solution for talking to AWS is the `amazonka` package so far.

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
