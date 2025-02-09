;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; search.lisp --- search packages

(uiop:define-package #:vix/src/search
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/search)

;;; TODO
;;; vix search -n 'foo|bar'
;;; vix search nixpkgs 'foo|bar'
(def search/command ()
  "search command"
  (clingon:make-command
   :name "search"
   :description "search packages"
   :usage "[option...]"
   :handler (lambda (cmd)
              (let ((args (clingon:command-arguments cmd)))
                (nrun "search" "nixpkgs" (pipe-args args))))
   :examples (mini-help "Search for packages with `fox' or `fire' in name"
                        "vix search fox fire")))
