;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;; build.lisp --- build the vix executable

(require 'asdf)
(defun cwd-name ()
  (multiple-value-bind (type list x y)
      (uiop:split-unix-namestring-directory-components
       (namestring (uiop:getcwd)))
    (declare (ignore type x y))
    (car (last list))))
(defun cwd-keyword () (intern (cwd-name) (find-package :keyword)))
(defun home (path) (merge-pathnames path (user-homedir-pathname)))
#-quicklisp (load (home "quicklisp/setup.lisp"))
(push (uiop:getcwd) asdf:*central-registry*)
(ql:quickload (cwd-keyword))
(asdf:make (cwd-keyword))
(uiop:quit)
