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
        (opt-nixpkgs (clingon:getopt cmd :opt-nixpkgs)))
    (if opt-nixpkgs
        (nrun "search" "nixpkgs" (or-args args))
        (nrun "search" args))))

(define-command nil search (s)
  "search for packages"
  "[-n]"
  (search/options)
  #'search/handler
  "Search in Nixpkgs for packages named `firefox'"
  "search -n firefox"
  "Search in Nixpkgs for packages underneath the attribute `gnome3'"
  "search nixpkgs#gnome3 vala")
