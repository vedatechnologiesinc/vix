;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; driver-tests.lisp --- symbol driver tests

(uiop:define-package :vix/t/driver-tests
  (:nicknames #:vix/t)
  (:use #:uiop/common-lisp
        #:marie)
  (:use-reexport #:vix/t/core-tests))

(provide "vix/t")
(provide "VIX/T")
