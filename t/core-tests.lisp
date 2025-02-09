;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; core-tests.lisp -- core functions tests

(uiop:define-package #:vix/t/core-tests
  (:use #:cl #:marie
        #:fiveam
        #:vix))

(in-package #:vix/t/core-tests)

(def run-tests ()
  "Run all the tests defined in the suite."
  (run-all-tests))
