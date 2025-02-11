;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; key.lisp --- generate and convert Nix signing keys

(uiop:define-package #:vix/src/key
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/key)

(define-command key convert^key-convert ()
  "generate a public key for verifying store paths"
  nil nil nil
  "Convert a secret key to a public key"
  "key-convert foo")

(define-command key generate^key-generate ()
  "generate a secret key for signing store paths"
  nil nil nil
  "Generate a new secret key"
  "key-generate --key-name cache.example.org-1 > ./secret-key")
