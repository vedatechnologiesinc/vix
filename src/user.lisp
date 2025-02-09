;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; user.lisp --- user sandbox

(uiop:define-package #:vix/src/user
  (:nicknames #:vix-user)
  (:use #:cl
        #:marie
        #:vix/src/driver))

(in-package #:vix-user)
