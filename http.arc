(declare 'atstrings t)

(def http-gen-req-header (host (o path "/") (o method "GET") (o accept "*/*") (o agent "curl/7.64.1"))
  (+ "@method @path HTTP/1.1\r\n"
     "Host: @{host}\r\n"
     "User-Agent: @{agent}\r\n"
     "Accept: @{accept}\r\n"
     "\r\n"))

(def http-parse-response (bytes)
  (withs ((hdr _ body) (or (partition (coerce "\r\n\r\n" 'bytes) bytes)
                           (partition (coerce "\n\n" 'bytes) bytes)
                           (err "invalid response" (cut (coerce bytes 'string) 0 100)))
          hdr (lines:coerce hdr 'string)
          (version _ status) (partition " " (car hdr))
          h (listtab:map [let (lh rh) (halve _ #\space)
                           (list (sym:downcase:cut lh 0 -1)
                                 (cut rh 1))]
                         (cdr hdr)))
    (= h!version version
       h!status status)
    (list h body)))

(def http-fetch-bytes (url (o path "/") (o accept "*/*"))
  (withs ((host (o port "80")) (tokens url #\:)
          port (int port)
          (i o) (w/values (tcp-connect host port))
          hdr (http-gen-req-header url path accept))
    (disp hdr o)
    (close o)
    (after (drain:readb i)
      (close i))))

(def http-fetch (url (o path "/") (o accept "*/*"))
  (aand (http-fetch-bytes url path accept)
        (http-parse-response it)))


;arc> (aand (http-fetch "example.com") (car it))
;'#hash((age . "524940")
;       (cache-control . "max-age=604800")
;       (content-length . "1256")
;       (content-type . "text/html; charset=UTF-8")
;       (date . "Sat, 03 Dec 2022 05:19:06 GMT")
;       (etag . "\"3147526947+ident\"")
;       (expires . "Sat, 10 Dec 2022 05:19:06 GMT")
;       (last-modified . "Thu, 17 Oct 2019 07:18:26 GMT")
;       (server . "ECS (cha/80C2)")
;       (status . "200 OK")
;       (vary . "Accept-Encoding")
;       (version . "HTTP/1.1")
;       (x-cache . "HIT"))
;arc> (aand (http-fetch "example.com") (coerce (cadr it) 'string 'utf8) (ellipsize it))
;"<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n\n    <meta chars..."
