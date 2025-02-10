;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; specials.lisp --- special variables

(uiop:define-package #:vix/src/specials
  (:use #:cl
        #:marie))

(in-package #:vix/src/specials)


;;; main variables

(defk +project-name+
  "vix"
  "The name of the project.")

(defk +project-version+
  "0.0.1"
  "The version number of the project.")
