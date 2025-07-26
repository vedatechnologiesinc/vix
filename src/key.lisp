;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; key.lisp --- generate and convert Nix signing keys

(uiop:define-package #:vix/src/key
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/key)

(define-command key convert (con)
  "generate a public key for verifying store paths"
  "<key>"
  nil
  t
  nil
  "Convert a secret key to a public key"
  "k con foo")

(define-command key generate (gen)
  "generate a secret key for signing store paths"
  ""
  nil
  t
  nil
  "Generate a new secret key"
  "k gen -- --key-name cache.example.org-1 > ./secret-key")

(define-command nil key ()
  "generate and convert Nix signing keys"
  "<command>"
  nil
  #'print-usage
  (convert generate))
