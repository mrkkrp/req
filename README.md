# Req

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/req.svg?style=flat)](https://hackage.haskell.org/package/req)
[![Stackage Nightly](http://stackage.org/package/req/badge/nightly)](http://stackage.org/nightly/package/req)
[![Stackage LTS](http://stackage.org/package/req/badge/lts)](http://stackage.org/lts/package/req)
[![Build Status](https://travis-ci.org/mrkkrp/req.svg?branch=master)](https://travis-ci.org/mrkkrp/req)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/req/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/req?branch=master)

* [Motivation and Req vs other libraries](#motivation-and-req-vs-other-libraries)
* [Unsolved problems](#unsolved-problems)
* [Related packages](#related-packages)
* [Contribution](#contribution)
* [License](#license)

This is an easy-to-use, type-safe, expandable, high-level HTTP library that
just works without any fooling around.

What does the “easy-to-use” phrase mean? It means that the library is
designed to be beginner-friendly, so it's simple to add it to your monad
stack, intuitive to work with, well-documented, and does not get in your
way. Doing HTTP requests is a common task and Haskell library for this
should be very approachable and clear to beginners, thus certain compromises
were made. For example, one cannot currently modify `ManagerSettings` of
default manager because the library always uses the same implicit global
manager for simplicity and maximal connection sharing. There is a way to use
your own manager with different settings, but it requires a bit more typing.

“Type-safe” means that the library is protective and eliminates certain
class of errors. For example, we have correct-by-construction URLs, it's
guaranteed that user does not send request body when using methods like GET
or OPTIONS, amount of implicit assumptions is minimized by making user
specify his/her intentions in explicit form (for example, it's not possible
to avoid specifying body or method of a request). Authentication methods
that assume TLS force user to use TLS on type level. The library carefully
hides underlying types from lower-level `http-client` package because it's
not safe enough (for example `Request` is an instance of `IsString` and if
it's malformed, it will blow up at run-time).

“Expandable” refers to the ability of the library to be expanded without
ugly hacking. For example, it's possible to define your own HTTP methods,
new ways to construct body of request, new authorization options, new ways
to actually perform request and how to represent/parse response. As user
extends the library to satisfy his/her special needs, the new solutions work
just like built-ins. That said, all common cases are covered by the library
out-of-the-box.

“High-level” means that there are less details to worry about. The library
is a result of my experiences as a Haskell consultant, working for several
clients who have very different projects and so the library adapts easily to
any particular style of writing Haskell applications. For example, some
people prefer throwing exceptions, while others are concerned with purity:
just define `handleHttpException` accordingly when making your monad
instance of `MonadHttp` and it will play seamlessly. Finally, the library
cuts boilerplate considerably and helps write concise, easy to read and
maintain code.

The library uses the following mature packages under the hood to guarantee
you best experience without bugs or other funny business:

* [`http-client`](https://hackage.haskell.org/package/http-client) — low
  level HTTP client used everywhere in Haskell.

* [`http-client-tls`](https://hackage.haskell.org/package/http-client-tls) —
  TLS (HTTPS) support for `http-client`.

It's important to note that since we leverage well-known libraries that the
whole Haskell ecosystem uses, there is no risk in using Req, as the
machinery for performing requests is the same as with `http-conduit` and
Wreq, it's just the API is different.

## Motivation and Req vs other libraries

*This section is my opinion and it contains criticisms of other well-known
libraries. If you're user/fan of one of these libraries, please remember not
to react aggressively and respect the fact that I may have different views
on API design from yours.*

I have spent time to write the library because sending HTTP requests is such
a common thing and still there is no high-level library for that in Haskell
that I could use with pleasure. I'll explain why.

First of all there is `http-client` and `http-client-tls`. They just work. I
have no issues with the libraries except that they are too low-level for my
taste. Indeed, even the docs say that they are low-level and “intended as a
base layer for more user-friendly packages”. This is exactly how I use them
in Req, as base level. Req is nothing but a different API to `http-client`,
so it only works because of hard work put into `http-client`.

`http-conduit` definitely has its place. For one thing it allows you to
stream request and response bodies in constant memory, what other library
allows you to do that? On the other hand if you take a look at
`Network.HTTP.Simple`, then although it's said that it's a “higher level
API”, it's mostly the same as vanilla `http-client` in spirit/approach and
just adds `conduit`-powered functions to perform requests and allows to use
global implicit `Manager` (super-cool idea, BTW, Req does the same). If I
tried to frame what exactly I don't like about `http-conduit` in words, then
it would be “the way requests are constructed”. You set, set, set instead of
*being forced* to declare necessary bits and *being allowed* to declare
optional bits in a way that their combination is certainly valid. And you
parse request from a string without the protection of TH that otherwise
saves the day as in Yesod.

Then there is Wreq.
`wreq`
[doesn't see much development lately](https://github.com/bos/wreq/issues/93).
`wreq` is by itself a weird library, IMO. You have functions per method —
not very good, as there may be new methods, like PATCH which is not new but
still missing (well you have `customMethod`, but what is the point of having
per-method functions if you have a more general way to use any method? you
should be able to just insert methods in the “argument slot” of
`customMethod` and end up with a more general solution). Now every method
function has a companion that takes `Options` (like you have `get` and
`getWith`). Why the duplication? Where is generality and flexibility? This
is not all though, because you cannot really use `get` you see in the main
module, because you want to have connection sharing. Wreq's author does not
take the gift of automatic connection re-use `Manager` from `http-client`
provides, he invents the whole new thing of “sessions”. Only inside a
session your connections will be shared and re-used. However with the
session stuff you have yet another set of per-method functions like `get`
and `getWith` — these are different ones, to be used with sessions! Now if
you have multi-threaded app, here is a surprise for you: you can't share
connections between threads as connections are shared only inside
`withSession` friend and “session will no longer be valid after that
function returns”. Disclaimer: I don't use Wreq, see below. If something in
this paragraph is not correct, please let me know and I'll remove it. Also
there are valid uses for sessions, but the point is that they are too
inconvenient for common tasks.

It's funny that one client I worked for had to have his own little wrapper
around `http-client` just because he could not possibly use `wreq` and
`http-client` and friends were too low-level. The previous paragraph is
extracted from a talk with a Haskell developer who works for that client. I
thought to myself “something is wrong with HTTP client libraries in Haskell
if they had to make a wrapper”.

What else? I used `servant-client` a couple of times but amount of
boilerplate is too high. If you have several query parameters, and you use
just one of them, good luck passing lots of `Nothing`s.

## Unsolved problems

AWS request signing is problematic because request body can be in form of
action to execute (and all that “popper” stuff for streaming), not just
`ByteString` and so getting its digest (hash) is not trivial without running
the action and consuming body in its entirety before the request in made. In
Wreq the author chose to just use `error` when body is not a (strict or
lazy) `ByteString`. Maybe it's OK for Wreq, but I don't consider this proper
solution for Req as we support full variety of body options. For example
what if I want to upload 1 Gb file to S3? I want to stream it in constant
memory but at the same time I need to calculate its hash before I start
streaming. One solution to the problem seems to be in taking the hash
explicitly (as an argument of hypothetical `awsAuth`) and making it
responsibility of library user to calculate the hash correctly. I don't like
this because it's not user-friendly. So the question stays open, for now
there is no AWS signing functionality provided out-of-the-box.

## Related packages

The following packages are designed to be used with Req:

* [`req-conduit`](https://hackage.haskell.org/package/req-conduit) — support
  for streaming request and response bodies in constant memory.

If you happen to have written a package that adds new features to Req,
please submit a PR to include it in this list.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/req/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2016 Mark Karpov

Distributed under BSD 3 clause license.
