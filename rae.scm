#lang scheme
(require net/http-easy)

(define input (car (vector->list (current-command-line-arguments))))

(define get-byte-index (lambda (byte bytes)
  (cond
    ((bytes=? bytes #"") 1)
    ((eqv? (bytes-ref bytes 0) byte) 0)
    (else (+ 1 (get-byte-index byte (subbytes bytes 1 (bytes-length bytes))))))))

(define get-bytes-index (lambda (bytes1 bytes2)
  (let
    ((bytes1-length (bytes-length bytes1))
    (bytes2-length (bytes-length bytes2)))
    (cond
      ((> bytes1-length bytes2-length) bytes2-length)
      ((bytes=? bytes1 (subbytes bytes2 0 bytes1-length)) 0)
      (else (+ 1 (get-bytes-index bytes1 (subbytes bytes2 1 bytes2-length))))))))

(define get-page (lambda (word)
    (response-body
      (get (string-append "https://dle.rae.es/" word "?m=form")))))

(define remove-tags (lambda (bytes)
  (let
    ((<-index (get-byte-index 60 bytes))
    (>-index (get-byte-index 62 bytes))
    (text-length (bytes-length bytes)))
    (cond
      ((bytes=? #"" bytes) #"")
      ((or (>= <-index text-length) (>= >-index text-length)) bytes)
      (else (bytes-append
        (subbytes bytes 0 <-index)
        (remove-tags (subbytes bytes (+ >-index 1) text-length))))))))

(define get-article (lambda (bytes)
  (let
    ((article-begin (get-bytes-index #"<article" bytes))
    (article-end (get-bytes-index #"</article>" bytes)))
    (subbytes bytes article-begin article-end))))

(define show-word-info (lambda (word)
  (display (remove-tags (get-article (get-page word))))))

(show-word-info input)