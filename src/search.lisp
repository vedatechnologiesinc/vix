;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; search.lisp --- search packages

(uiop:define-package #:vix/src/search
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/search)

(def- search/options ()
  "Returns the options for the `search' command."
  (list
   (clingon:make-option :flag
                        :description "toggle Nixpkgs"
                        :short-name #\n
                        :long-name "nixpkgs"
                        :required nil
                        :key :opt-nixpkgs)))

(def- search/handler (cmd)
  "The handler for the `search' command"
  (let ((args (clingon:command-arguments cmd))
        (state (clingon:getopt cmd :opt-nixpkgs)))
    (if state
        (nrun "search" "nixpkgs" (or-args args))
        (nrun "search" args))))

(define-command nil search ()
  "search for packages"
  "[-n]"
  (search/options)
  #'search/handler
  "Search in Nixpkgs for packages with `fire' or `fox' in name"
  "vix search -n fire fox")
