;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; rebuild.lisp --- rebuild the system

(uiop:define-package #:vix/src/rebuild
  (:use #:cl
        #:marie
        #:vix/src/core))

(in-package #:vix/src/rebuild)

(defv- *flake* (uiop:native-namestring (home "etc/dev/")))

(def- rebuild/handler (cmd)
  "Handler for the rebuild command"
  (let ((args (clingon:command-arguments cmd)))
    (uiop:os-cond
     ((uiop:os-macosx-p) (run! `("darwin-rebuild" "--flake" ,*flake* ,@args)))
     ((uiop:os-unix-p) (run! `("sudo" "nixos-rebuild" "--flake" ,*flake* ,@args)))
     (t (clingon:print-usage-and-exit cmd t)))))

(define-command rebuild nil rebuild
  "rebuild the system"
  nil
  #'rebuild/handler
  "Rebuild the system from flake"
  "vix rebuild"
  "Rebuild the system from flake and switch to it"
  "vix rebuild switch")
