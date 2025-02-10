;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; flake.lisp --- direct nix flake commands

(uiop:define-package #:vix/src/config
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/config)

(define-command config config-show (cs)
  "show the Nix configuration or the value of a specific setting"
  nil nil nil
  "Show configuration"
  "config-show")

(define-command config config-check (cc)
  "check your system for potential problems and print a PASS or FAIL for each check"
  nil nil nil
  "Check for problems"
  "config-check")
