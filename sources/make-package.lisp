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

#+sbcl
(progn 
  (import '(; sb-ext::load-foreign
	    sb-pcl:slot-definition-initargs
	    sb-pcl:slot-definition-initform
	    sb-pcl:slot-definition-name
	    sb-pcl:class-direct-slots
	    sb-pcl:class-slots
	    sb-pcl:class-direct-superclasses
	    sb-pcl:generic-function-name
	    sb-mop:class-direct-subclasses
	    sb-mop:validate-superclass
	    sb-sys:without-interrupts
	    )
	  :cm)
  (defun finalize-class (class) 
    (sb-pcl:finalize-inheritance class)))

