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
