;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; driver.lisp --- symbol driver

(uiop:define-package #:vix/src/driver
  (:nicknames #:vix)
  (:use #:uiop/common-lisp)
  (:use-reexport #:vix/src/specials
                 #:vix/src/core
                 #:vix/src/rebuild
                 #:vix/src/search
                 #:vix/src/profile
                 #:vix/src/flake
                 #:vix/src/develop
                 #:vix/src/etc
                 #:vix/src/registry
                 #:vix/src/config
                 #:vix/src/derivation
                 #:vix/src/hash
                 #:vix/src/key
                 #:vix/src/nar
                 #:vix/src/store
                 #:vix/src/main))

(provide "vix")
(provide "VIX")
