;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cl-user)

(defpackage :cm-patterns
  (:nicknames :cm)
  (:use :common-lisp)
  (:export :patterns :cycle :palindrome :line :heap :weighting
	   :markov :markov-analyze :graph :accumulation :thunk
	   :rotation :rewrite :range :join :pval :copier
	   :new :next :*pattern-version* :*pattern-date*))

(in-package :cm)

#+openmcl
(progn 
  (import '(ccl:class-slots
            ccl:slot-definition-initargs
            ccl:slot-definition-initform
            ccl:slot-definition-name
            ccl:class-direct-superclasses
            ccl:class-direct-subclasses
            ccl:class-direct-slots
            ccl:validate-superclass
            ccl:without-interrupts)
          :cm)
  (defun finalize-class (class) class t))


#+lispworks
(progn 
  (import '(clos:slot-definition-name 
            clos:slot-definition-initargs 
            clos:slot-definition-initform 
            clos:class-direct-superclasses
            clos:class-direct-subclasses
            clos:finalize-inheritance
            hcl:class-slots
            hcl:class-direct-slots
            hcl:validate-superclass
            mp:without-interrupts
            )
          :cm)
  (defun finalize-class (class) class (values)))
