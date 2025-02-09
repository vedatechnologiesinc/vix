;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; user-tests.lisp --- user sandbox tests

(uiop:define-package :vix/t/user-tests
  (:nicknames #:vix-tests-user)
  (:use #:cl #:marie
        #:vix/t/driver-tests))

(in-package #:vix-tests-user)
