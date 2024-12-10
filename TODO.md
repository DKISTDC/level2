
- [ ] Download - skip the step?
- [ ] Interface - Search/filter on /proposals/
- [ ] Interface - from proposals screen, ahrd to click in
- [ ] Fits Gen - reduce parallel down from 16, it's too slow
- [ ] Cryo-NIRSP - Invalid instrument


BUGS
-----

- [ ] Inversion - upload error, see below
- [ ] /proposal/N - should be /proposals/ ??


Error after upload

    VanillaHttpException (HttpExceptionRequest Request {
      host                 = "transfer.api.globus.org"
      port                 = 443
      secure               = True
      requestHeaders       = [("Authorization","<REDACTED>"),("Accept","application/json")]
      path                 = "/v0.10/task/f855c1e6-b729-11ef-b839-115107518ebc"
      queryString          = ""
      method               = "GET"
      proxy                = Nothing
      rawBody              = False
      redirectCount        = 10
      responseTimeout      = ResponseTimeoutDefault
      requestVersion       = HTTP/1.1
      proxySecureMode      = ProxySecureWithConnect
    }
    (StatusCodeException (Response {responseStatus = Status {statusCode = 404, statusMessage = "Not Found"}, responseVersion = HTTP/1.1, responseHeaders = [("Server","nginx"),("Date","Tue, 10 Dec 2024 19:07:16 GMT"),("Content-Type","application/json"),("Transfer-Encoding","chunked"),("Connection","keep-alive"),("x-transfer-api-version","0.10"),("x-transfer-api-koa-version","5.8"),("cache-control","no-cache"),("pragma","no-cache"),("expires","0"),("x-transfer-api-error","ClientError.NotFound"),("Access-Control-Allow-Origin","*"),("Content-Encoding","gzip")], responseBody = (), responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose, responseOriginalRequest = Request {
      host                 = "transfer.api.globus.org"
      port                 = 443
      secure               = True
      requestHeaders       = [("Authorization","<REDACTED>"),("Accept","application/json")]
      path                 = "/v0.10/task/f855c1e6-b729-11ef-b839-115107518ebc"
      queryString          = ""
      method               = "GET"
      proxy                = Nothing
      rawBody              = False
      redirectCount        = 10
      responseTimeout      = ResponseTimeoutDefault
      requestVersion       = HTTP/1.1
      proxySecureMode      = ProxySecureWithConnect
    }
    , responseEarlyHints = []}) "{\n  \"code\": \"ClientError.NotFound\",\n  \"message\": \"No task found with id 'f855c1e6-b729-11ef-b839-115107518ebc'\",\n  \"request_id\": \"ZJJXZkYJj\",\n  \"resource\": \"/task/f855c1e6-b729-11ef-b839-115107518ebc\"\n}"))

