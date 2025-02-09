;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; echo.lisp --- echo things right back

(uiop:define-package #:vix/src/echo
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/echo)

(def- echo/handler (cmd)
  "Handler for the `echo' command"
  (dolist (arg (clingon:command-arguments cmd))
    (format t "~A~&" arg)))

(def echo/command ()
  "Creates a new command which echoes every argument we are given"
  (clingon:make-command
   :name "echo"
   :usage "[arguments ...]"
   :description "echo back each argument on a newline"
   :handler #'echo/handler
   :examples '(("Echo back each argument on a new line:" . "vix echo foo bar baz"))))
