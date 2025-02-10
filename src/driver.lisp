;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; driver.lisp --- symbol driver

(uiop:define-package #:vix/src/driver
  (:nicknames #:vix)
  (:use #:uiop/common-lisp)
  (:use-reexport #:vix/src/core
                 #:vix/src/rebuild
                 #:vix/src/search
                 #:vix/src/profile
                 #:vix/src/flake
                 #:vix/src/develop
                 #:vix/src/main))

(provide "vix")
(provide "VIX")
