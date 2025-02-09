;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; main.lisp --- main entry point

(uiop:define-package #:vix/src/main
  (:use #:cl
        #:marie
        #:vix/src/core
        #:vix/src/rebuild
        #:vix/src/profile
        #:vix/src/flake
        #:vix/src/search
        #:vix/src/dev))

(in-package #:vix/src/main)


;;; variables

(defk- +project-name+
  "vix"
  "The name of the project.")

(defk- +project-version+
  "0.0.0"
  "The version number of the project.")


;;; fns

(def- dot/command ()
  "Return the command for the `dot' command."
  (clingon:make-command
   :name "dot"
   :description "generate tree representation in Dot format"
   :usage ""
   :handler (lambda (cmd)
              (let ((parent (clingon:command-parent cmd)))
                (clingon:print-documentation :dot parent t)))))

(def- top-level/options ()
  "Return the options for the top-level command."
  (list
   (clingon:make-option :counter
                        :description "Verbosity"
                        :short-name #\v
                        :long-name "verbose"
                        :key :verbose)))

(def- top-level/sub-commands ()
  "Returns the list of sub-commands for the top-level command"
  (list
   ;; rebuild
   (rebuild/command)

   ;; search
   (search/command)

   ;; profile
   (install/command)
   (uninstall/command)
   (upgrade/command)
   (list/command)
   (rollback/command)
   (history/command)
   (wipe-history/command)
   (diff-closures/command)

   ;; flake
   (init/command)
   (metadata/command)
   (show/command)
   (update/command)
   (new/command)
   (clone/command)
   (check/command)
   (archive/command)
   (prefetch/command)

   ;; develop
   (dev/command)
   (command/command)
   (make/command)
   ))

(def- top-level/handler (cmd)
  "The handler for the top-level command. Prints the command usage."
  (clingon:print-usage-and-exit cmd t))

(def- top-level/command ()
  "Return the top-level command"
  (clingon:make-command
   :name "vix"
   :version +project-version+
   :description "vix"
   :long-description (fmt "Vix is a program for interacting with the Nix ecosystem")
   :authors '("Rommel Martínez <ebzzry@icloud.com>")
   :handler #'top-level/handler
   :options (top-level/options)
   :sub-commands (top-level/sub-commands)))

(def main (&rest args)
  "The main entry point of the program."
  (let ((app (top-level/command)))
    (handler-case (clingon:run app)
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl ccl:interrupt-signal-condition
       #+clisp system::simple-interrupt-condition
       #+ecl ext:interactive-interrupt
       #+allegro excl:interrupt-signal
       #+lispworks mp:process-interrupt
       () nil)
      (error (c)
        (format t "Oof, an unknown error occured:~&~A~&" c)))))
