;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; uninstall.lisp --- uninstall packages

(uiop:define-package #:vix/src/uninstall
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/uninstall)

(def uninstall/command ()
  "uninstall command"
  (clingon:make-command
   :name "uninstall"
   :description "uninstall packages"
   :usage "[arguments ...]"
   :handler (lambda (cmd)
              (let ((args (clingon:command-arguments cmd)))
                (loop :for arg :in args
                      :do (nrun "profile" "remove" arg))))
   :examples '(("Uninstall package `rsync':" . "vix uninstall rsync"))))
