;;;; -*- mode: lisp; syntax: common-lisp; base: 10; -*-
;;;; vix-tests.asd --- test ASDF file for vix

(defsystem #:vix-tests
    :name "vix-tests"
    :description "Test ASDF file of vix"
    :version #.(uiop:read-file-form (make-pathname :directory '(:relative "t") :name "version" :type "lisp"))
    :author "VEDA Technologies, Inc. <hello@veda-tech.com>"
    :license "BSD-2-Clause"
    :class :package-inferred-system
    :depends-on (#:fiveam
                 #:marie
                 #:vix
                 #:vix/t/core-tests
                 #:vix/t/driver-tests
                 #:vix/t/user-tests)
    :perform (test-op (o c) (uiop:symbol-call :vix/t/core-tests :run-tests)))
