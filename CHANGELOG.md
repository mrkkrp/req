## Req 0.2.0

* Added support for multipart form data in form of `ReqBodyMultipart` body
  option and `reqBodyMultipart` helper function. This also required a change
  in type signature of `getRequestContentType`, which now takes `body`, not
  `Proxy body` because we need to extract boundary from `body` and put it
  into `Content-Type` header. This change, however, shouldn't be too
  dangerous for end-users.

## Req 0.1.0

* Initial release.
