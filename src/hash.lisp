;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; hash.lisp --- compute and convert cryptographic hashes

(uiop:define-package #:vix/src/hash
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/hash)

(define-command hash file^hash-file ()
  "print hash of a regular file"
  nil nil nil
  "Print hash of `file.txt'"
  "hash-file file.txt")

(define-command hash path^hash-path ()
  "print hash of the NAR serialisation of a path"
  nil nil nil
  "Print hash of path `/foo/bar/'"
  "hash-path /foo/bar/")

(define-options hash-convert)
(define-handler hash-convert ("hash" "convert"))
(define-command hash convert^hash-convert ()
  "convert between hash formats"
  nil t t
  "Convert a hash"
  "hash-convert --hash-algo sha1 800d59cfcd3c05e900cb4e214be48f6b886a08df")
