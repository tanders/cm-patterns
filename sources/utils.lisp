;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cm)


(defun quote-if-necessary (x)
  (if (or (numberp x)
          (stringp x)
          (vectorp x)
          (and (consp x) (eq (car x) 'quote)))
      x
      `',x))

(defun slot-init-forms (o &key eval omit only key ignore-defaults)
  (loop
     for s in (class-slots (class-of o))
     for n = (slot-definition-name s)
     for k = (slot-definition-initargs s)
     for v = (if (slot-boundp o n) (slot-value o n) ':unbound-slot)
     when (and (not (eq v ':unbound-slot))
	       (not (null k))
	       (if omit
		   (not (member n omit :test #'eq))
		   (if only
		       (member n only :test #'eq)
		       t))
	       (not (and ignore-defaults
			 (eq v (slot-definition-initform s)))))
     collect (car k)
     and collect (if key
		     (funcall key v)
		     (if eval
			 (quote-if-necessary v)
			 v))))

(defmacro dopairs (decl &body body)
  (let* ((m "dopairs: (v1 v2 list [return]) . body")
         (s (if (consp decl) (pop decl) (error m)))
         (v (if (consp decl) (pop decl) (error m)))
         (l (if (consp decl) (pop decl) (error m)))
         (x (if (consp decl) (pop decl) nil))
         (a (gensym))
         (h (gensym)))
    `(let ((,h ,l))
       (do ((,a ,h (cddr ,a)) (,s nil) (,v nil))
           ((null ,a) ,x)
         (setf ,s (car ,a))
         (if (null (cdr ,a))
             (error "Uneven pair list: ~s" ,h)
	     (setf ,v (cadr ,a)))
         ,@body))))

(defun symbol->keyword (sym)
  (let ((str (symbol-name sym)))
    (or (find-symbol str ':keyword)
        (intern str :keyword))))

(defun expand-inits (class args)
  (let* ((slots (class-slots class))
         (inits (list nil))
         (tail1 inits)
         (save args))
    (do ((sym nil) (val nil) (slot nil))
        ((null args) (cdr inits))
      (setf sym (pop args))
      (setf val (if (null args)
		    (error "Uneven initialization list: ~s" save)
		    (pop args)))
      (cond ((typep sym 'keyword) t)
            ((and sym (symbolp sym)) (setf sym (symbol->keyword sym)))
            (t (error "'~s' is not an initialization for ~s: ~s." sym class save)))
      (setf slot
            (find-if (lambda (x)
                       (member sym (slot-definition-initargs x)))
                     slots))
      (if slot
          (progn
            (rplacd tail1 (list sym val))
            (setf tail1 (cddr tail1)))
	  (error "'~s' is not an initialization for ~s." sym class)))))

(defmacro new (class &body args)
  (let* ((type (or (find-class class) (error "No class named ~s." class)))
         (inits (expand-inits type args)))
    `(make-instance (find-class ',class) ,@inits)))

;; (defun new (class &rest args)
;;   (let* ((type (or (find-class class) (error "No class named ~s." class)))
;;          (inits (expand-inits type args)))
;;     (apply #'make-instance (find-class class) inits)))


