## Req 3.8.0

* Adjusted the value of the `httpConfigRetryJudgeException` field of
  `defaultHttpConfig` to retry on response timeouts and connection timeouts.

## Req 3.7.0

* Added `reqCb`, a function that allows you to modify the `Request` object
  but otherwise performs the requst for you.

* Derived `MonadThrow`, `MonadCatch`, and `MonadMask` for the `Req` monad.

## Req 3.6.0

* Added the `httpConfigBodyPreviewLength` configuration parameter to
  `HttpConfig`.

## Req 3.5.0

* Made `Req` an instance of `MonadUnliftIO`. [Issue
  100](https://github.com/mrkkrp/req/issues/100).

## Req 3.4.0

* Requests using `DELETE` method can now have a body. [Issue
  89](https://github.com/mrkkrp/req/issues/89).

* Added the `httpConfigRetryJudgeException` field to `HttpConfig` so that
  requests that result in exceptions can be retried. [Issue
  93](https://github.com/mrkkrp/req/issues/93).

* Added the function `renderUrl`. [Issue
  83](https://github.com/mrkkrp/req/issues/83).

## Req 3.3.0

* Derived `Show` instances for response types `IgnoreResponse`,
  `JsonResponse`, `BsResponse`, and `LbsResponse`.

## Req 3.2.0

* Made the tests pass with `http-client-0.7` and later.

* Added a quasiquoter for URL creation, `urlQ`.

## Req 3.1.0

* Changed signature of `httpConfigRetryPolicy` to `RetryPolicyM IO`.

## Req 3.0.0

* Dropped functions `parseUrlHttp`, `parseUrlHttps`, and `parseUrl`. Instead
  we now have `useHttpURI`, `useHttpsURI`, and `useURI` take `URI`s from
  `modern-uri` as their argument. You first parse your URL with the
  `modern-uri` package and then pass it to those functions. This allows us
  to work with typed URI representations and seamlessly convert them to
  something `req` can work with. As a side effect basic auth from the `URI`s
  is now taken into consideration. In the future we may also start to
  respect fragments if `http-client` starts to support this.

* Dropped support for GHC 8.2 and older.

## Req 2.1.0

* Dropped support for GHC 7.10.

* Added the new `acceptHeader` method to the `HttpResponse` type class.
  Notably, the `jsonResponse` method now sets `"Accept"` header to
  `"application/json"`.

## Req 2.0.1

* Fixed the `httpbin` tests (they changed something on the server again).

## Req 2.0.0

* Got rid of `data-default-class` dependency, now we export
  `defaultHttpConfig` instead.

## Req 1.2.1

* Fixed a typo in the type signature of `parseUrl`.

## Req 1.2.0

* Added the `parseUrl` function.

## Req 1.1.0

* Added `customAuth` and `attachHeader` to facilitate creation of custom
  authentication options.

* Added `basicProxyAuth` authentication option.

## Req 1.0.0

* Added the `reqBr` function allowing to consume `Response BodyReader`
  without using a pre-defined instance of `HttpResponse`, in a custom way.

* Now streaming of response body does not happen until we've checked headers
  and status code with `httpConfigCheckResponse`. It also doesn't happen on
  every retry. Streaming and obtaining of final response value happens only
  once when we're happy with everything.

  Previously we first tried to consume and interpret response body before
  checking status code and determining whether we should retry the request.
  This was not good, because we could expect a JSON response but get a
  response with status code 500, and then still we would try to parse it as
  JSON first before letting `httpConfigCheckResponse` throw an exception.

  The corrected behavior should also make retrying more efficient.

* Changed signatures of several fields of `HttpConfig`:
  `httpConfigCheckResponse`, `httpConfigRetryPolicy`, and
  `httpConfigRetryJudge` in order to eliminate redundant `IO` and prevent
  the possibility that these functions could start consuming `BodyReader`.

* Removed the `makeResponsePreview` method from the `HttpResponse` type
  class. Preview business is handled by the library automatically on a lower
  level now. Users do not need to concern themselves with such stuff.

* Changed the type signature of the `getHttpResponse` method of the
  `HttpResponse` type class. Previously it left too much freedom (and
  responsibility) to implementers of the method. In fact, we now limit what
  `getHttpResponse` does to just consuming and interpreting `Response
  BodyReader`, so we can properly control details of connection
  opening/closing etc., for the user.

* Dropped support for GHC 7.8.

* Minor documentation improvements.

## Req 0.5.0

* Changed the signature of the `makeResponseBodyPreview` from `response ->
  IO ByteString` to `response -> ByteString`.

* Minor documentation improvements.

## Req 0.4.0

* Added the `Req` monad and `runReq` function to run it. This allows to use
  `req` without defining new (orphan) instances.

## Req 0.3.1

* Added `basicAuthUnsafe`.

## Req 0.3.0

* Made URL parsing functions `parseUrlHttp` and `parseUrlHttps` recognize
  port numbers.

* Added `req'` function that allows to perform requests via a callback that
  receives pre-constructed request and manager.

* Removed the `ReturnRequest` HTTP response implementation as it was not
  quite safe and was not going to work with retrying. Use `req'` instead for
  “pure” testing.

* Changed the type of `httpConfigCheckResponse`, so the second argument can
  be any instance of `HttpResponse`.

* Added built-in automatic retrying. See `httpConfigRetryPolicy` and
  `httpConfigRetryJudge` in `HttpConfig`. The default configuration retries
  5 times on request timeouts.

* Added the `makeResponseBodyPreview` method to the `HttpResponse` type
  class that allows to specify how to build a “preview” of response body for
  inclusion into exceptions.

* Improved wording in the documentation and `README.md`.

## Req 0.2.0

* Added support for multipart form data in the form of `ReqBodyMultipart`
  body option and `reqBodyMultipart` helper function. This also required a
  change in the type signature of `getRequestContentType`, which now takes
  `body`, not `Proxy body` because we need to extract boundary from `body`
  and put it into `Content-Type` header. This change, however, shouldn't be
  too dangerous for end-users.

* Added support for OAuth 1.0 authentication via `oAuth1` option.

## Req 0.1.0

* Initial release.
