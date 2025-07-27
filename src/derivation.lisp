;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; derivation.lisp --- work with derivations

(uiop:define-package #:vix/src/derivation
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/derivation)

(define-command derivation add ()
  "add a store derivation"
  "<path>"
  nil
  t
  nil
  "Add a derivation"
  "drv dadd path")

(define-command derivation show ()
  "show the contents of a store derivation"
  "<derivation>"
  t
  t
  nil
  "Show the `hello' derivation"
  "drv dshow n#hello")

(define-command nil derivation (drv)
  "work with derivations"
  "<command>"
  nil
  #'print-usage
  (add show))
