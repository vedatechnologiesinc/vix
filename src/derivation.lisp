;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; derivation.lisp --- work with derivations

(uiop:define-package #:vix/src/derivation
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/derivation)

(define-command derivation dadd ()
  "add a store derivation"
  "<path>"
  nil
  t
  nil
  "Add a derivation"
  "v dadd path")

(define-command derivation dshow ()
  "show the contents of a store derivation"
  "<derivation>"
  t
  t
  nil
  "Show the `hello' derivation"
  "v dshow n#hello")

(define-command nil derivation (v)
  "work with derivations"
  "<command>"
  nil
  #'print-usage
  (dadd dshow))
