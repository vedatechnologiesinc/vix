;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; config.lisp --- config the Nix settings

(uiop:define-package #:vix/src/config
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/config)

(define-command config show ()
  "show the Nix configuration or the value of a specific setting"
  ""
  nil
  t
  nil
  "Show configuration"
  "conf show")

(define-command config check ()
  "check your system for potential problems"
  ""
  nil
  t
  nil
  "Check for problems"
  "conf k")

(define-command nil config (conf)
  "manage the Nix settings" "<command>"
  nil
  #'print-usage
  (show check))
