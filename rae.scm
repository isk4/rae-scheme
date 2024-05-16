#lang scheme
(require net/http-easy)

(define input (car (vector->list (current-command-line-arguments))))

(define get-char-index (lambda (char str)
  (cond
    ((string=? str "") 1)
    ((eqv? (string-ref str 0) char) 0)
    (else (+ 1 (get-char-index char (substring str 1 (string-length str))))))))

(define get-str-index (lambda (str1 str2)
  (let
    ((str1-length (string-length str1))
    (str2-length (string-length str2)))
    (cond
      ((> str1-length str2-length) str2-length)
      ((string=? str1 (substring str2 0 str1-length)) 0)
      (else (+ 1 (get-str-index str1 (substring str2 1 str2-length))))))))

(define get-page (lambda (word)
  (bytes->string/utf-8
    (response-body
      (get (string-append "https://dle.rae.es/" word "?m=form"))))))

(define remove-tags (lambda (text)
  (let
    ((<-index (get-char-index #\< text))
    (>-index (get-char-index #\> text))
    (text-length (string-length text)))
    (cond
      ((string=? "" text) "")
      ((or (>= <-index text-length) (>= >-index text-length)) text)
      (else (string-append
        (substring text 0 <-index)
        (remove-tags (substring text (+ >-index 1) text-length))))))))

(define get-article (lambda (text)
  (let
    ((article-begin (get-str-index "<article" text))
    (article-end (get-str-index "</article>" text)))
    (substring text article-begin article-end))))

(define show-word-info (lambda (word)
  (display (remove-tags (get-article (get-page word))))))

(show-word-info input)