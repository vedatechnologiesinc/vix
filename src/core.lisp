;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; core.lisp --- core functions

(uiop:define-package #:vix/src/core
  (:use #:cl
        #:marie))

(in-package #:vix/src/core)


;;; utils

(def run! (cmd)
  "Run CMD."
  (uiop:run-program cmd :input :interactive :output :interactive :error-output :interactive))

(def nrun (&rest args)
  "Use the command `nix' to run ARGS."
  (run! (append (list "nix") (flatten-list args))))

(def pipe-args (args)
  "Return a string from ARGS suitable for the `install' command."
  (format nil "~{nixpkgs#~A~^ ~}" args))

(def or-args (args)
  "Return a string from ARGS suitable for the `search' command."
  (format nil "~{~A~^|~}" args))

(def mini-help (&rest args)
  "Return a list suitable for the example usage of a command."
  (let ((parts (partition args 2)))
    (loop :for (desc usage) :in parts
          :collect (cons (fmt "~A:" desc) usage))))

(def cmd-handler (cmd fn)
  "Define a function that parses the command arguments from CCMD and runs the
Nix command CMD from it."
  (let ((args (clingon:command-arguments cmd)))
    (funcall fn args)))


;;; entry point

(defm define-command (sname fname aliases
                      desc usage options handler
                      &rest examples)
  "Return a function for CLINGON:MAKE-COMMAND."
  (let* ((sname-name (when sname
                       (string-downcase (prin1-to-string sname))))
         (fname-name (prin1-to-string fname))
         (aliases-name (when aliases
                         (mapcar #'(lambda (alias)
                                     (string-downcase (string alias)))
                                 aliases)))
         (suffix-name (cat fname-name "/COMMAND"))
         (function-name (read-from-string suffix-name)))
    `(def ,function-name ()
       (clingon:make-command
        :name ,(string-downcase fname-name)
        :aliases ',aliases-name
        :description ,desc
        :usage (if (null ,usage) "[option...]" ,usage)
        :options ,options
        :handler (if (null ,handler)
                     (lambda (cmd)
                       (let ((args (or (clingon:command-arguments cmd) ""))
                             (sub (or ,sname-name "")))
                         (nrun sub ,(string-downcase fname-name) args)))
                     ,handler)
        :examples (mini-help ,@examples)))))
