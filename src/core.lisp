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

(def nixpkgs-args (args)
  "Return a string from ARGS suitable for `install'."
  (format nil "~{nixpkgs#~A~^ ~}" args))

(def pipe-args (args)
  "Return a string from ARGS suitable for `search'."
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

(defm define-command (sname cname fname desc usage handler &rest examples)
  "Return a function for CLINGON:MAKE-COMMAND."
  (let* ((sname-name (string-downcase (prin1-to-string sname)))
         (cname-name (if cname (string-downcase (prin1-to-string cname)) ""))
         (command-name (prin1-to-string fname))
         (suffix-name (cat command-name "/COMMAND"))
         (function-name (read-from-string suffix-name)))
    `(def ,function-name ()
       (clingon:make-command
        :name ,(string-downcase command-name)
        :description ,desc
        :usage (if (null ,usage) "[option...]" ,usage)
        :handler (if (null ,handler)
                     (lambda (cmd)
                       (let ((args (or (clingon:command-arguments cmd) "")))
                         (nrun ,sname-name ,cname-name args)))
                     ,handler)
        :examples (mini-help ,@examples)))))
