;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :cm)

(defparameter +constant-data+ (ash 1 0))
(defparameter +constant-weights+ (ash 1 1))
(defparameter +count-periods+ (ash 1 2))
(defparameter +count-values+ (ash 1 3))
(defparameter +depth-first+ (ash 1 4))
(defparameter +breadth-first+ (ash 1 5))
(defparameter +range-stepping+ (ash 1 10))
(defparameter +range-initially+ (ash 1 11))
(defparameter +range-unbounded+ (ash 1 12))
(defparameter +range-dynamic+ (ash 1 13))
(defparameter +constant-minmax+ (ash 1 14))
(defparameter +default-period+ (ash 1 15))
(defparameter +range-random+ (ash 1 16))
;; (defparameter +range-random+ (ash 1 17))
(defparameter +nad+ (quote :not-a-datum))
(defparameter +eop+ (quote :end-of-period))
(defparameter +eod+ (quote :end-of-data))

;;; the period struct holds information for various period calculations.  count
;;; is number of reads remaining in current period. when count=0 the period is
;;; reinitialized. length is maximum count of the period, either a number or T
;;; if dynamic length. if stream is not nil it a new length will be read from it
;;; each time the period is initialized.  omit is the number of times this
;;; stream is skipped in its parent's pattern, if dynamic. Reps keeps track of
;;; the number of periods. Max is the max number of periods allowed, after which
;;; the pattern always returns +exhausted+

(defstruct period (count 0) length stream default (omit 0) (reps 0) (hook nil))

(progn
  (defclass container ()
    ((name :initform nil :accessor object-name :initarg :name)))
  (defparameter <container> (find-class 'container))
  (finalize-class <container>))



(progn
  (defclass pattern (container)
    ((flags :initarg :flags :initform 0 :accessor pattern-flags)
     (data :initarg :of :initform '() :accessor pattern-data)
     (length :accessor pattern-length)
     (datum :initform +nad+ :accessor pattern-datum)
     (period :initarg :for :accessor pattern-period)
     (value :initarg :last-value :initform +nad+ :accessor pattern-value)
     (state :initarg :last-state :initform +nad+ :accessor pattern-state)
     (repeat :initarg :repeat :initform most-positive-fixnum :accessor pattern-repeat)
     (parser :initarg :parser :initarg :through :initarg :from :initarg :in :initform nil :accessor pattern-parser)
     (hooks :initarg :eop-hook :initform nil :accessor pattern-hooks)
     (returning :initform nil :initarg :returning :accessor pattern-returning)
     (counting :initarg :counting :initform :periods :accessor pattern-counting)
     (traversing :initarg :traversing :initform :depth-first :accessor pattern-traversing)))
  (defparameter <pattern> (find-class 'pattern))
  (finalize-class <pattern>))

;;;
;;; canonicalize-pattern-data returns three values: a list of parsed data, the
;;; number of elements parsed, and T or NIL if the data is constant (contains no
;;; substreams).
;;;

(defun maybeparse (fn val)
  (if (or (not fn) (typep val <pattern>)) val (funcall fn val)))

(defmethod canonicalize-pattern-data ((obj pattern) data parser inits)
  (values data (length data) (not (some #'pattern? data))))

;;;
;;; make-load-form "decompiles" pattern
;;;

(defmethod make-load-form ((obj pattern) &optional env)
  (let ((inits (pattern-external-inits obj))
        (gvar
         (intern (string-upcase (format nil "<~a>" (class-name (class-of obj)))) :cm)))
    `(make-instance ,gvar ,@inits)))

;;;
;;; pattern-external-inits returns the list of slot inits to include in a
;;; make-load-form expression. every subclass of pattern should call
;;; (next-method) and append any local inits to that list.
;;;

(defmethod pattern-external-inits ((obj pattern))
  (let ((inits (slot-init-forms obj
				:eval t
				:ignore-defaults t
				:only '(repeat parser returning counting traversing)
				:key #'expand-pattern-value)))
    (unless (logtest (pattern-flags obj) +default-period+)
      (let ((per (pattern-period obj)))
        (push (expand-pattern-value
               (or (period-stream per) (period-length per)))
              inits)
        (push ':for inits)))
    inits))

(defun expand-pattern-value (val)
  (cond ((pattern? val) (make-load-form val))
        ((consp val)
         (if (eq (first val) 'quote)
             val
	     (cons 'list
		   (loop
		      for x in val
		      collect (expand-pattern-value x)))))
        ((and val (symbolp val)) `',val)
        (t val)))

;;;
;;; pattern-period-length returns the current period length
;;;

(defmethod pattern-period-length ((obj pattern))
  (period-length (pattern-period obj)))

;;;
;;; default-period-length returns the default period length for a given pattern
;;; class. defaults to the to the number of elements the pattern contains.
;;;

(defmethod default-period-length ((obj pattern)) (pattern-length obj))

(defmethod initialize-instance :after ((obj pattern) &rest args)
  (let ((flags (pattern-flags obj))
        (data nil)
        (hook (pattern-hooks obj))
        len parser type
	;; optn valu
	constant?)
    (do ((a args (cddr a)))
        ((null a) nil)
      (case (car a)
        ((:of) (setf data (cadr a)))
        ((:in :from :to :through)
	 (cond ((null type))
	       (t (error "Option ~S does not match data type ~S." (car a) type)))
	 ;; (setf optn (car a))
	 ;; (setf valu (cadr a))
	 )))
    (unless (listp data) (setf data (list data)))
    (multiple-value-setq (data len constant?)
      (canonicalize-pattern-data obj data parser args))
    (setf (pattern-data obj) data)
    (setf (pattern-length obj) len)
    (let ((counting (pattern-counting obj)))
      (when counting
        (case counting
          ((:periods :period periods period)
           (setf flags (logior flags +count-periods+)))
          ((:values :value values value)
           (setf flags (logior flags +count-values+)))
          (t (error "~s not one of 'counting' keywords:  periods values." counting)))))
    (let ((traversing (pattern-traversing obj)))
      (when traversing
        (case traversing
          ((:depth-first :depth depth-first depth)
           (setf flags (logior flags +depth-first+)))
          ((:breadth-first :breadth breadth-first breadth)
           (setf flags (logior flags +breadth-first+)))
          (t
           (error "~s not 'traversing' keword: depth-firs,t breadth-first."
                  traversing)))))
    (when constant? (setf flags (logior flags +constant-data+)))
    (cond ((logtest flags +count-values+)
           (setf flags (logand flags (lognot +count-periods+))))
          (t
           (if (logtest flags +constant-data+)
               (setf flags
                     (logior (logand flags (lognot +count-periods+))
                             +count-values+))
	       (setf flags (logior flags +count-periods+)))))
    (let* ((default (default-period-length obj))
           (period
            (if (slot-boundp obj 'period)
                (or (pattern-period obj) default)
		default))
           (repeat (pattern-repeat obj)))
      (when (eq repeat t)
        (setf (pattern-repeat obj) most-positive-fixnum))
      (if (and (slot-boundp obj 'period)
               (not (null (slot-value obj 'period))))
          (setf period (slot-value obj 'period))
	  (progn
	    (setf period default)
	    (setf flags (logior flags +default-period+))))
      (setf (pattern-period obj)
            (if (or (numberp period) (eq period t))
                (make-period :length period :default default :hook hook)
		(make-period :stream period :default default :hook hook))))
    (setf (pattern-flags obj) flags)))

;;;
;;; type predicate for patterns.
;;;
(defmethod pattern? (obj) obj nil)
(defmethod pattern? ((obj pattern)) obj)
;;;
;;; Predicates for testing end-of-period and end-of-data.
;;;

(defmethod eop? (x) (eq x +eop+))
(defmethod eop? ((x pattern)) (eop? (pattern-state x)))
(defmethod eod? (x) (eq x +eod+))
(defmethod eod? ((x pattern)) (eod? (pattern-value x)))
(defmethod pattern-state (obj) obj +eop+)

;;;
;;; next returns the next value read from the object.  This around method
;;; implements the basic behavior of patterns.
;;;
;;; It first checks the stream's period length and calls reset-period if at end.
;;; If the next period length is 0 it immediately returns +nad+, which causes a
;;; superior stream (if any) to skip over the current stream as it increments
;;; its pattern.  Otherwise, the method then increments the streams pattern
;;; until it yields a datum that is not +nad+ and that call-next-method does not
;;; return +nad+ from.
;;;
;;; If the stream's data is known to contain only constant values, ie no
;;; substreams, the testing loop is skipped. once call-next-method returns a
;;; value (not +nad+), the period and pattern of the stream are incremented
;;; according to their mode.
;;;
;;; For period incrementing, +count-periods+ increments the period count only
;;; on +eop+, and +count-values+ increments the period count every time. for
;;; pattern incrementing, +depth-first+ increments the pattern only on +eop+,
;;; and +breadth-first+ increments the pattern every time.
;;;

(defun next (obj &optional num)
  (if num
      (cond ((numberp num) (let ((l (list nil)))
			     (do ((i 0 (+ 1 i))
				  (e l (cdr e)))
				 ((>= i num) (cdr l))
			       (rplacd e (list (next-1 obj))))))
	    ((pattern? obj) (let ((l (list nil)))
			      (do ((n (next-1 obj))
				   (e l (cdr e))
				   (f nil))
				  ((or (eq n +eod+) f) (cdr l))
				(rplacd e (list n))
				(if (eop? obj)
				    (setf f t)
				    (setf n (next-1 obj))))))
	    (t (list obj)))
      (next-1 obj)))

(defmethod next-1 (obj) obj)

(defmethod next-1 ((obj function)) (funcall obj))
;; (defmethod next-1 ((obj function)) obj)

(defmethod next-1 ((obj pattern))
  (let ((period (pattern-period obj))
	(nomore nil))
    (when (= (period-count period) 0)
      (when (>= (period-reps period) (pattern-repeat obj))
        (setf (pattern-value obj) +eod+)
        (setf (pattern-state obj) +eop+)
        (setf nomore +eod+))
      (when (and (not nomore) (= (reset-period obj) 0))
        (setf nomore +nad+)
        (setf (pattern-value obj) +nad+)
        (setf (pattern-state obj) +eop+)))
    (if nomore
        nomore
	(let ((flags (pattern-flags obj))
	      (retfn (pattern-returning obj))
	      (value nil)
	      (state nil))
	  (if (logtest flags +constant-data+)
	      (progn
		(setf (pattern-datum obj) (next-in-pattern obj))
		(setf value (next-1 (pattern-datum obj)))
		(setf state +eop+))
	      (loop
		 with dyn? = (and (logtest flags +count-periods+)
				  (eq (period-length period) t))
		 do
		   (loop
		      while (eq (pattern-datum obj) +nad+)
		      do (setf (pattern-datum obj)
			       (if dyn?
				   (skip-datum?
				    (next-in-pattern obj))
				   (next-in-pattern obj))))
                   (setf value (next-1 (pattern-datum obj)))
                   (setf state (pattern-state (pattern-datum obj)))
                   (if (eq value +nad+)
                       (setf (pattern-datum obj) value)
		       (return))))
	  (cond ((eq state +eop+)
		 (setf (period-count period) (- (period-count period) 1))
		 (setf (pattern-datum obj) +nad+)
		 (setf state nil))
		(t (when (logtest flags +breadth-first+)
		     (setf (pattern-datum obj) +nad+))
		   (when (logtest flags +count-values+)
		     (setf (period-count period)
			   (- (period-count period) 1)))))
	  (if (= (period-count period) 0)
	      (progn
		(setf state +eop+)
		(setf (period-reps period) (+ 1 (period-reps period))))
	      (setf state state))
	  (if retfn (setf value (funcall retfn value)))
	  (setf (pattern-state obj) state)
	  (setf (pattern-value obj) value)
	  value))))

;;;
;;; skip-datum? returns +nad+ if the current stream should be skipped in the
;;; pattern. this only happens if we have dynamic periodicity and the datum had
;;; a 0 length period when it was encountered by reset-period.
;;;

(defmethod skip-datum? ((obj pattern))
  (let ((period (pattern-period obj)))
    (if (> (period-omit period) 0)
        (progn
          (setf (period-omit period) (- (period-omit period) 1))
          +nad+)
	obj)))

(defmethod skip-datum? (obj) obj)

;;;
;;; reset-period sets and returns the length of the next period.  period length
;;; of constant datum is always 1.
;;;

(defmethod reset-period (obj) obj 1)

(defmethod reset-period ((obj pattern))
  (let ((period (pattern-period obj))
	(dyn nil)
	(len nil))
    ;; if period is supplied as a stream get next length via item
    (when (period-stream period)
      (setf (period-length period) (next-1 (period-stream period))))
    (setf dyn (eq (period-length period) t))
    (setf len
          (if dyn
	      (period-default period)
	      (period-length period)))
    ;; if we have dynamic period length we adjust next period length
    ;; for the number of 0 subperiods that this period will encounter.
    ;; in order for this to work, all substream periods must be reset
    ;; now, at the same that the super stream is reset. we can only
    ;; do this if we know that all subperiods are currently at end
    ;; of period, ie if we are counting by subperiods. if so, then by
    ;; definition all the substreams must be at end-of-period  or we
    ;; couldn't have gotton here in the first place. after resetting
    ;; substream period lengths we decrement our current stream's period
    ;; length by the number of zero periods found.
    (when (and dyn (logtest (pattern-flags obj) +count-periods+))
      (let ((zeros 0))
        (map-pattern-data
         #'(lambda (x)
	     (when (= (reset-period x) 0)
	       (let ((p (pattern-period x)))
		 (setf (period-omit p) (+ (period-omit p) 1)))
	       (incf zeros)))
         obj)
        (when (> zeros 0) (setf len (max (- len zeros) 0)))))
    (setf (period-count period) len)
    (let ((hook (period-hook period)))
      (when hook (funcall hook)))
    len))

;;;
;;; pattern implementations.
;;;
;;; cycle continously loops over its data. the data are held
;;; in a list of the form: (data . data). successive elements are
;;; popped from the cdr, when the cdr is null it's reset to the car.
;;;


(defun make-cycl () (make-list 2))
(defun cycl-data (cycl) (car cycl))

(defmacro cycl-data-set! (cycl data) `(rplaca ,cycl ,data))
(defun cycl-last (cycl) (cadr cycl))
(defmacro cycl-last-set! (cycl data) `(rplaca (cdr ,cycl) ,data))
(defun cycl-tail (cycl) (cddr cycl))
(defmacro cycl-tail-set! (cycl tail) `(rplacd (cdr ,cycl) ,tail))
;; (defmacro pop-cycl (cycl) `(pop (cdr (cdr ,cycl))))
(defun pop-cycl (cycl) (pop (cdr (cdr cycl))))

(defmacro reset-cycl (cycl)
  (let ((c (gensym)))
    `(let ((,c ,cycl)) (cycl-tail-set! ,c (car ,c)))))

(progn
  (defclass cycle (pattern) nil)
  (defparameter <cycle> (find-class 'cycle))
  (finalize-class <cycle>))

(defmethod pattern-external-inits ((obj cycle))
  (append (list ':of
                (cons 'list
                      (loop
			 for x in (car (pattern-data obj))
			 collect (expand-pattern-value x))))
          (call-next-method)))

(defmethod initialize-instance :after ((obj cycle) &rest args)
  (let ((cyc (make-cycl)))
    (cycl-data-set! cyc (pattern-data obj))
    (setf (pattern-data obj) cyc)))

(defmethod next-in-pattern ((obj cycle))
  (let ((cyc (pattern-data obj)))
    (when (null (cycl-tail cyc))
      (cycl-tail-set! cyc (cycl-data cyc)))
    (pop-cycl cyc)))

(defmethod map-pattern-data (fn (obj cycle))
  (map nil fn (cycl-data (pattern-data obj))))

;;;
;;; palindrome visits the reverse of its data.
;;;

(progn
  (defclass palindrome (pattern)
    ((elide :initform nil :initarg :elide :accessor palindrome-elide)))
  (defparameter <palindrome> (find-class 'palindrome))
  (finalize-class <palindrome>))


(defmethod pattern-external-inits ((obj palindrome))
  (append (list ':of
                (cons 'list
                      (loop
			 for x in (car (pattern-data obj))
			 collect (expand-pattern-value x))))
          (call-next-method)
          (if (not (palindrome-elide obj))
              (list)
	      (list ':elide (expand-pattern-value (palindrome-elide obj))))))

(defmethod map-pattern-data (fn (obj palindrome))
  (map nil fn (cycl-data (pattern-data obj))))

(defmethod default-period-length ((obj palindrome))
  (* 2 (pattern-length obj)))

(defmethod initialize-instance :after ((obj palindrome) &rest args)
  (let ((cyc (make-cycl)))
    (cycl-data-set! cyc (pattern-data obj))
    (setf (pattern-data obj) cyc)))

(defmethod next-in-pattern ((obj palindrome))
  (let ((cycl (pattern-data obj)))
    (when (null (cycl-tail cycl))
      (let ((mode (next-1 (palindrome-elide obj)))
            (half (cycl-data cycl))
            (next '())
            (long (pattern-length obj))
            (bits (pattern-flags obj)))
        (cond ((member mode '(t 2) :test #'eq)
               (setf long (- (* long 2) 2))
               (setf next (append half (cdr (reverse (cdr half))))))
              ((member mode '(nil 0) :test #'eq)
               (setf long (* long 2))
               (setf next (append half (reverse half))))
              ((member mode '(-1 :first first :start start :left left) :test #'eq)
               (setf long (- (* long 2) 1))
               (setf next (append half (reverse (cdr half)))))
              ((member mode '(1 :last last :end end :right right) :test #'eq)
               (setf long (- (* long 2) 1))
               (setf next (append half (cdr (reverse half)))))
              (t (error "~s is not an elide value: ~s, ~s, :first, :last." mode t nil)))
        (when (logtest bits +default-period+)
          (setf (period-count (pattern-period obj)) long))
        (cycl-tail-set! cycl next)))
    (pop-cycl cycl)))


;;;
;;; line sticks on the last element.
;;;

(progn
  (defclass line (pattern) ())
  (defparameter <line> (find-class 'line))
  (finalize-class <line>))

(defmethod pattern-external-inits ((obj line))
  (append (list ':of
                (cons 'list
                      (loop
			 for x in (car (pattern-data obj))
			 collect (expand-pattern-value x))))
          (call-next-method)))

(defmethod initialize-instance :after ((obj line) &rest args)
  (declare (ignore args))
  (let ((cyc (make-cycl)))
    (cycl-data-set! cyc (pattern-data obj))
    ;; set the tail only this one time
    (cycl-tail-set! cyc (cycl-data cyc))
    (setf (pattern-data obj) cyc)))

(defmethod next-in-pattern ((obj line))
  (let ((cycl (pattern-data obj)))
    ;; if no cdr then car is last item
    (if (null (cdr (cycl-tail cycl)))
        (car (cycl-tail cycl))
	(pop-cycl cycl))))

(defmethod reset-period :before ((obj line))
  (when (null (cdr (cycl-tail (pattern-data obj))))
    (setf (pattern-length obj) 1)))

(defmethod map-pattern-data (fn (obj line))
  (map nil fn (cycl-data (pattern-data obj))))

;;;
;;; heap is a cycle that shuffles its elements each time through
;;;

(progn
  (defclass heap (cycle)
    ((random-state :initform *random-state* :initarg :state :accessor pattern-random-state)
     ;; Added by Anders Vinjar
     (elide-last? :initform nil :initarg :elide-last? :accessor heap-elide-last?
		  :documentation "[Boolean] If true, ban direct repetition of any previous item. If nil, this may happen when crossing period-boundaries")))
  (defparameter <heap> (find-class 'heap))
  (finalize-class <heap>))

(defmethod pattern-external-inits ((obj heap))
  (let ((inits (call-next-method))
	(state (pattern-random-state obj))
	(elide-last? (heap-elide-last? obj)))
    (append inits
	    (and state (list :state state))
	    (and elide-last? (list :elide-last? elide-last?)))))

(defmethod initialize-instance :after ((obj heap) &rest args)
  (let ((cyc (pattern-data obj)))
    (cycl-data-set! cyc (copy-list (cycl-data cyc)))))

(defmethod next-in-pattern ((obj heap))
  (flet ((shufl (lis len state)
           (loop
	      for i below len
	      for j = (random len state)
	      for v = (elt lis i)
	      do
		(setf (elt lis i) (elt lis j))
		(setf (elt lis j) v))
           lis))
    (let ((cyc (pattern-data obj)))
      (when (null (cycl-tail cyc))
	(cycl-tail-set! cyc
			(if (heap-elide-last? obj)
			    (loop for new-cycle = (shufl
						   (cycl-data cyc)
						   (pattern-length obj)
						   (pattern-random-state obj))
			       while (equal (car new-cycle) (heap-elide-last? obj))
			       finally (return new-cycle))
			    (shufl
			     (cycl-data cyc)
			     (pattern-length obj)
			     (pattern-random-state obj)))))
      (if (heap-elide-last? obj)
	  (setf (heap-elide-last? obj) (pop-cycl cyc))
	  (pop-cycl cyc)))))

#|
(let ((hhh (make-instance 'heap :of '(0 1 2) :elide-last? nil)))
  (loop repeat 20 collect (next-in-pattern hhh)))

(let ((hhh (make-instance 'heap :of '(0 1 2) :elide-last? t)))
  (loop repeat 20 collect (next-in-pattern hhh)))
|#

;;;
;;; weighting chooses using weighted selection. its data are kept in a list of
;;; the form: ((&rest choices) . last-choice).
;;;


(defparameter *random-range* nil)

(defstruct random-item datum index (weight 1) (min 1) max (count 0) id minmax)

(progn
  (defclass weighting (pattern)
    ((range :initform *random-range* :accessor random-pattern-range)
     (random-state :initform *random-state* :initarg :state :accessor pattern-random-state)
     (adjustable :initform nil :initarg :adjustable :accessor weighting-adjustable)))
  (defparameter <weighting> (find-class 'weighting))
  (finalize-class <weighting>))

(defmethod pattern-external-inits ((obj weighting))
  (flet ((fnc (n) (let ((dat (expand-pattern-value (random-item-datum n)))
			(lst (list)))
		    ;; push in reverse order.
		    (unless (null (random-item-max n))
		      (push (expand-pattern-value (random-item-max n)) lst)
		      (push ':max lst))
		    (unless (or (not (numberp (random-item-min n)))
				(= (random-item-min n) 1))
		      (push (expand-pattern-value (random-item-min n)) lst)
		      (push ':min lst))
		    (unless (and (numberp (random-item-weight n))
				 (= (random-item-weight n) 1))
		      (push (expand-pattern-value (random-item-weight n)) lst)
		      (push ':weight lst))
		    (if (null lst)
			dat
			(cons 'list (cons dat lst))))))
    (let ((lst (call-next-method)))
      (append (list :of (cons 'list
			      (loop for x in (car (pattern-data obj))
				 collect (fnc x))))
              lst
              (if (eq *random-state* (pattern-random-state obj))
                  '()
		  (list :state (pattern-random-state obj)))))))

(defmethod default-period-length ((obj weighting))
  ;; set the default period length of an adjustable or all-subpattern weighting
  ;; to 1 else to the number of elements. since a weighting pattern establishes
  ;; no particular order itself, setting the period to 1 allows the number of
  ;; elements in the current period to reflect the sub patterns.  A better
  ;; defaulting would be to check mixed elements at run time and adjust the
  ;; period length accordingly,

  (if (weighting-adjustable obj)
      1
      (do ((tail (pattern-data obj) (cdr tail)) (flag t))
	  ((or (not flag) (null tail))
	   (if flag
	       1
	       (pattern-length obj)))
	(setf flag (pattern? (random-item-datum (car tail)))))))

(defmethod initialize-instance :after ((obj weighting) &rest args)
  (let ((pool (pattern-data obj))
        (sum (if (integerp *random-range*) 0 0.0))
        (adj (weighting-adjustable obj))
        (const t))
    (loop
       for item in pool
       for min = (random-item-min item)
       for max = (random-item-max item)
       unless (and (or (not min) (numberp min))
		   (or (not max) (numberp max)))
       do (progn
	    (setf const nil)
	    (setf (random-item-min item) nil)
	    (setf (random-item-max item) nil)
	    (setf (random-item-minmax item) (cons min max))))
    (when const
      (setf (pattern-flags obj) (logior +constant-minmax+ (pattern-flags obj))))
    ;; check the stream for constant weights. if true, calculate the range now
    ;; and set a flag so we dont recalulate each period.
    (loop
       for item in pool
       for weight = (random-item-weight item)
       while sum
       if (numberp weight)
       do (progn
	    (incf sum weight)
	    (setf (random-item-index item) sum))
       else do (setf sum nil)
       finally (when (and sum *random-range*)
		 (dolist (item pool)
		   (setf (random-item-index item)
			 (* (/ (random-item-index item) sum) *random-range*)))))
    (cond ((not sum) (when adj
		       (error "Found non-numeric weight in adjustable weighting.")))
          (t (unless *random-range* (setf (random-pattern-range obj) sum))
	     (unless adj (setf (pattern-flags obj)
			       (logior +constant-weights+
				       (pattern-flags obj))))))
    ;; All routines treat pool as: ((&rest choices) . last-choice) no initial
    ;; last choice.  A first choice for the stream could be implemented as a last
    ;; with min=1
    (setf (pattern-data obj) (list pool))))


(defmethod canonicalize-pattern-data ((obj weighting) data parser inits)
  (declare (ignore obj inits))
  (flet ((parse-random-item (extern)
           (apply #'(lambda (datum &rest keys)
		      (setf datum (maybeparse parser datum))
		      (loop
			 with orig = keys
			 and args = '() and key and val
			 while (not (null keys))
			 do
			   (setf key (pop keys))
			   (setf val (if keys
					 (pop keys)
					 (error "Uneven weighting list: ~s." orig)))
			   (push val args)
			   (case key
			     ((weight :weight) (push ':weight args))
			     ((min :min) (push ':min args))
			     ((max :max) (push ':max args))
			     (t (error "~s not one of: :weight, :min, :max." key)))
			 finally (return (apply #'make-random-item :datum datum args))))
                  (if (consp extern)
		      extern
		      (list extern)))))
    (let ((intern (mapcar #'parse-random-item data)))
      (values intern
              (length intern)
              (not (some #'(lambda (x)
			     (pattern? (random-item-datum x)))
                         intern))))))

(defun reset-random-range (obj range adj?)
  (let ((data (car (pattern-data obj))))
    (loop
       with s = (if (integerp range) 0 0.0)
       for item in data
       do (incf s (if adj?
		      (random-item-weight item)
		      (next-1 (random-item-weight item))))
	 (setf (random-item-index item) s)
       finally (if range
		   (dolist (i data)
		     (setf (random-item-index i)
			   (* (/ (random-item-index i) s) range)))
		   (setf (random-pattern-range obj) s)))))

(defmethod reset-period ((obj weighting))
  (let ((reset (call-next-method))
	(flags (pattern-flags obj)))
    (unless (logtest flags +constant-minmax+)
      (let ((b nil))
        (dolist (i (car (pattern-data obj)))
          (setf b (random-item-minmax i))
          (when b
            (setf (random-item-min i) (next-1 (car b))
		  (random-item-max i) (next-1 (cdr b)))))))
    (unless (logtest flags +constant-weights+)
      (loop
	 with s = (if (integerp *random-range*) 0 0.0)
	 for item in (car (pattern-data obj))
	 do
	   (incf s (next-1 (random-item-weight item)))
	   (setf (random-item-index item) s)
	 finally (if *random-range*
		     (dolist (i (car (pattern-data obj)))
		       (setf (random-item-index i)
			     (* (/ (random-item-index i) s) *random-range*)))
		     (setf (random-pattern-range obj) s))))
    reset))

(defmethod next-in-pattern ((obj weighting))
  ;; pool is ((&rest choices) . last-choice)
  (let* ((pool (pattern-data obj))
	 (last (cdr pool)))
    (if (and (not (null last))
             (progn
               (setf (random-item-count last) (+ 1 (random-item-count last)))
               (< (random-item-count last) (random-item-min last))))
        (random-item-datum last)
	(let ((range (random-pattern-range obj))
	      (state (pattern-random-state obj))
	      (choices (car pool))
	      (adjust (weighting-adjustable obj))
	      (next nil))
	  (unless (zerop range)				    ;pattern-data='(nil)
	    (setf next
		  (loop
		     for item = (loop
				   with index = (random range state)
				   for x in choices
				   when (< index (random-item-index x))
				   return x)
		     unless (and (random-item-max item)
				 (= (random-item-count item)
				    (random-item-max item)))
		     return item))
	    (unless (eq next last)
	      (dolist (i choices) (setf (random-item-count i) 0)))
	    (rplacd pool next)
	    ;; adjust the weight of the newly selected item
	    (when adjust
	      (setf (random-item-weight next)
		    (* (random-item-weight next) adjust)))
	    (random-item-datum next))))))

(defmethod map-pattern-data (fn (obj weighting))
  (map nil
       #'(lambda (x) (funcall fn (random-item-datum x)))
       (car (pattern-data obj))))

;;;
;;; markov 
;;;

(progn
  (defclass markov (pattern)
    ((past :initform '() :initarg :past :accessor markov-pattern-past)
     (order :accessor markov-pattern-order)
     (produce :initform nil :initarg :produce :accessor markov-pattern-produce)))
  (defparameter <markov> (find-class 'markov))
  (finalize-class <markov>))

(defmethod pattern-external-inits ((obj markov))
  ;; FIX --  past is not saved...
  (flet ((fnc (n) `',(append (first n)
			     (list '->)
			     (loop
				with s = 0
				for x in (cddr n)
				for w = (- (second x) s)
				collect (if (= w 1)
					    (car x)
					    (list (car x) w))
				do (setf s (second x))))))
    (append (list ':of
                  (cons 'list
                        (loop
			   for x in (cdr (pattern-data obj))
			   collect (fnc x))))
            (call-next-method)
            (if (not (markov-pattern-produce obj))
                (list)
		(list ':produce
		      (expand-pattern-value
		       (markov-pattern-produce obj)))))))

(defmethod canonicalize-pattern-data ((obj markov) data parser inits)
  (declare (ignore obj parser))
  (flet ((parse-markov-spec (spec)
           (let ((tail (or (member '-> spec) (member ':-> spec)))
                 (range 0)
                 (inputs '())
                 (parse '())
                 (outputs '()))
             (if tail
                 (progn
                   (setf inputs
                         (loop
			    until (eq spec tail)
			    collect (pop spec)))
                   (setf parse (cdr tail)))
		 (progn (setf inputs '()) (setf parse spec)))
             (dolist (s parse)
               (let ((val nil) (pat nil) (wei nil))
                 (if (consp s)
                     (progn
                       (setf val (first s))
                       (setf wei (if (null (cdr s)) 1 (second s)))
		       ;; weight may be number or pattern
                       (setf pat wei)
                       (unless (numberp wei) (setf wei nil)))
		     (progn (setf val s) (setf wei 1) (setf pat 1)))

                 ;; set range to nil if any weight is pattern else precalc range
                 ;; for the constant weights

                 (if (and wei range)
                     (incf range wei)
		     (setf range nil))
                 (push (list val range pat) outputs)))
             (cons inputs (cons range (reverse outputs))))))
    (let ((const t))
      (dopairs (a v inits)
	(case a ((:produce) (setf const (not (some #'pattern? v))))))
      (let ((coll (list nil)))
        (do ((tail data (cdr tail))
             (len 0 (+ len 1))
             (order nil)
             (lis coll)
             (p nil))
            ((null tail)
             (setf (markov-pattern-order obj) order)
             (values (cdr coll) len const))
          (setf p (parse-markov-spec (car tail)))
          (if (not order)
              (setf order (length (first p)))
	      (setf order (max order (length (first p)))))
          (rplacd lis (list p))
          (setf lis (cdr lis)))))))

(defmethod initialize-instance :after ((obj markov) &rest args)
  (declare (ignore args))
  (unless (consp (markov-pattern-past obj))
    (setf (markov-pattern-past obj)
          (make-list (markov-pattern-order obj) :initial-element '*))))

(defmethod next-in-pattern ((obj markov))
  ;; markov data kept as a list of lists. each list is in the form:
  ;; ((<inputs>) range . <output>)
  (labels ((select-output (range outputs)
	     ;; if range is false then one or more weights in the outputs are
	     ;; patterns. in this case we map all the outputs to update weights
	     ;; of every outcome and then select.  otherwise (range is number)
	     ;; we simply select an outcome from the precalculated distribution.
	     (if (not range)
                 (do ((tail outputs (cdr tail)) (out nil) (sum 0))
                     ((null tail) (select-output sum outputs))
		   ;; out is outcome: (val rng <pat/wei>)
                   (setf out (car tail))
                   (setf sum
                         (+ sum
                            (if (numberp (caddr out))
                                (caddr out)
				(next-1 (caddr out)))))
		   ;; always update second element to new value
                   (rplaca (cdr out) sum))
		 (let ((n (random range)))
		   (loop
		      for o in outputs
		      when (< n (second o)) return (first o)))))
           (match-past (inputs past)
             (loop
		for i in inputs
		for j in past
		unless (or (eq i '*) (equal i j) (eq j '*))
		return nil
		finally (return t))))
    (let ((past (markov-pattern-past obj))
          (data (markov-pattern-produce obj))
          (item nil))
      (loop
	 for i in (pattern-data obj)
	 when (or (null past) (match-past (first i) past))
	 do (return (let ((last nil))
		      (setf item
			    (select-output (second i) (cddr i)))
		      (unless (null past)
			(if (null (cdr past))
			    (rplaca past item)
			    (progn
			      ;; rotate past choices leftward
			      (setf last (last past))
			      (rplaca past item)
			      (rplacd last past)
			      (setf (markov-pattern-past obj)
				    (cdr past))
			      (rplacd (cdr last) '()))))
		      item))
	 finally (error "No outputs for past choices ~s."
			(markov-pattern-past obj)))
      (if data
          (let ((x (member item data))) (if x (second x) item))
	  item))))

;;;
;;; Markov analysis
;;;

;(define happy-birthday '(c4 c4 d4 c4 f4 e4 c4 c4 d4 c4 g4 f4
;			 c4 c4 c5 a4 f4 e4 d4 bf4 bf4 a4 f4 g4 f4))
;(markov-analyze happy-birthday :order 2)


(defun decimals (value places)
  (let ((n (expt 10.0 places))) (/ (round (* value n)) n)))

(defun markov-analyze (seq &key (order 1)
			     (print? t)			    ; t pattern table
			     (pattern? t)		    ; nil or pattern
			     sort?
			     (print-decimals 3) (period nil) key)
  (let ((len (length seq))
        (labels '())				      ; the set of all outcomes 
        (table '())
        (row-label-width 8)
        (pat nil)
        (field (+ print-decimals 2)))			    ; n.nnn 
    (labels ((add-outcome (prev next)
               (let ((entry
                      (find-if #'(lambda (x) (equal prev (car x)))
                               table)))
                 (if (not entry)
                     (push (list prev
                                 (format nil "~s" prev)
                                 (list next 1))
                           table)
		     (let ((e (assoc next (cddr entry))))
		       (if e
			   (rplaca (cdr e) (+ 1 (cadr e)))
			   (rplacd (last (cdr entry))
				   (list (list next 1))))))))
             (before? (x y l)
               (if (null x)
                   t
		   (let ((p1
			  (position-if #'(lambda (z) (equal (car x) z))
				       l))
			 (p2
			  (position-if #'(lambda (z) (equal (car y) z))
				       l)))
		     (cond ((< p1 p2) t)
			   ((= p1 p2) (before? (cdr x) (cdr y) l))
			   (t nil)))))
             (liststring (l)
               (if (null l)
                   ""
		   (let ((a (format nil "~s" (car l))))
		     (do ((x (cdr l) (cdr x)))
			 ((null x) a)
		       (setf a
			     (concatenate 'string
					  a
					  (format nil
						  " ~s"
						  (car x)))))))))
      (dotimes (i len)
        (loop
	   with prev = (list)
	   for j to order
	   for x = (let ((raw (elt seq (mod (+ i j) len))))
		     (if key (funcall key raw) raw))
	   when (< j order) do (push x prev)
	   finally (progn
		     (add-outcome (reverse prev) x)
		     (if (not (member x labels))
			 (push x labels)))))
      
      ;; sort the outcomes according to user specification: a list, a sorting
      ;; function or nil.
      
      (cond ((consp sort?) (setf labels sort?))
            (sort? (setf labels (sort labels sort?)))
            ((numberp (car labels)) (setf labels (sort labels #'<)))
            ((and (car labels)
                  (let ((t4 (car labels))) (and t4 (symbolp t4))))
             (setf labels
                   (sort labels
                         #'(lambda (x y)
			     (string-lessp (format nil "~a" x)
					   (format nil "~a" y))))))
            (t (setf labels (reverse labels))))
      ;; map over data, normalize weights 
      (loop
	 for row in table
	 for lab = (cadr row)				    ; label
	 for val = (cddr row)
	 maximize (length lab) into len
	 do (let ((total (loop for e in val sum (cadr e))))
	      (setf total (* total 1.0))
	      (loop
		 for e in val
		 do (rplaca (cdr e) (decimals (/ (cadr e) total) print-decimals))))
	 finally (setf row-label-width (max len row-label-width)))

      ;; sort table according to value order
      (setf table
            (sort table
                  #'(lambda (x y) (before? (car x) (car y) labels))))
      (when (member print? '(t table :table))
        (let* ((sp " ")
               (ln (make-string field :initial-element #\-)))
	  ;; print column header row
	  (progn
            (terpri)
	    ;; print left pad for row label
            (dotimes (i row-label-width) (write-char #\*))
            (dolist (l labels)
              (princ sp)
              (let* ((s (format nil "~a" l)) (n (length s)))
                (dotimes (i (max (- field n) 0))
                  (write-char #\Space))
                (princ s))))
          (dolist (row table)
            (terpri)
            (let* ((s (liststring (car row))) (n (length s)))
	      ;; pad number
              (dotimes (i (max (- row-label-width n) 0))
                (write-char #\Space))
              (dotimes (i (min row-label-width n))
                (write-char (elt s i))))
            (dolist (l labels)
              (let ((v (assoc l (cddr row))))
                (if (not v)
                    (progn (princ sp) (princ ln))
		    (let* ((s (prin1-to-string (cadr v)))
			   (n (length s)))
		      (princ sp)
		      (dotimes (i (max (- field n) 0))
			(write-char #\Space))
		      (princ s))))))
          (terpri))
        (force-output))
      (when (or pattern? (member print? '(t pattern :pattern)))
        (setf pat
              (loop
		 for row in table
		 collect (append (car row) '(->) (cddr row)))))
      (when (member print? '(t pattern :pattern))
        (pprint `(new markov of ',pat))
        (force-output))
      (when pattern?
	;; patterns not defined yet, cant use new or <markov>
	(make-instance (find-class 'markov) :of pat :for period)))))

;;;
;;; graph traverses its nodes by applying a selection
;;; function to the graph. data is list:
;;; (<node> . nodes)
;;; where <node> is the last selected node initialized to (first nodes)
;;;

(defstruct graph-node id datum to props)

(defun default-graph-node-select (obj node lastids)
  (let ((to (graph-node-to node)))
    (if (consp to)
        (if (eq (car to) ':idsel)
            (markov-select obj node to lastids)
	    (error ":to not id, :idsel or pattern: ~s." to))
	(next-1 to))))

(progn
  (defclass graph (pattern)
    ((selector :initform #'default-graph-node-select :initarg :selector :accessor graph-selector)
     (last :initform nil :initarg :last :accessor graph-last)
     (props :initform '() :initarg :props :accessor graph-props)
     (starting-node-index :initform 0 :initarg :starting-node-index :accessor graph-starting-node-index)))
  (defparameter <graph> (find-class 'graph))
  (finalize-class <graph>))

(defmethod pattern-external-inits ((obj graph))
  (flet ((fnc (n) (cons 'list
			(list (expand-pattern-value (graph-node-datum n))
			      ':id (graph-node-id n)
			      ':to (expand-pattern-value (graph-node-to n))))))
    (append (list ':of
                  (cons 'list
                        (loop
			   for x in (pattern-data obj)
			   collect (fnc x))))
            (call-next-method)
            (if (null (graph-props obj))
                (list)
		(list ':props (expand-pattern-value (graph-props obj))))
            (if (eq (graph-selector obj) #'default-graph-node-select)
                (list)
		(list ':selector (graph-selector obj))))))

(defmethod initialize-instance :after ((obj graph) &rest args)
  (let ((nodes (pattern-data obj)) (last (graph-last obj)))
    (when last
      (setf (graph-last obj)
            (if (consp last)
                (cons (length last) last)
		(cons last (make-list last :initial-element '*)))))
    (setf (pattern-data obj)
          (cons (elt nodes (graph-starting-node-index obj)) nodes)))
  (values))

(defmethod canonicalize-pattern-data ((obj graph) data parser inits)
  (flet ((parse-graph-item (extern)
           (unless (consp extern)
             (error "Graph node ~s not list." extern))
           (apply #'(lambda (datum &rest keys)
		      (do ((orig keys)
			   args id key val)
			  ((null keys)
			   (unless id
			     (push datum args)
			     (push ':id args))
			   (apply #'make-graph-node :datum (maybeparse parser datum) args))
			(setf key (pop keys))
			(setf val
			      (if (null keys)
				  (error "Bad graph node: ~s." orig)
				  (pop keys)))
			(push val args)
			(case key
			  ((id :id) (setf id t) (push ':id args))
			  ((to :to -> :->) (push ':to args))
			  ((props :props) (push ':props args))
			  (t (error "~s not one of: :id, :to, :props." key)))))
                  extern)))
    (let ((intern (mapcar #'parse-graph-item data)))
      (values intern
              (length intern)
              (not (some #'(lambda (x) (pattern? (graph-node-datum x)))
			 intern))))))

(defun markov-select (obj node table lastids)
  ;;  table is a list (:idsel <id1> <obj1> ...)
  (declare (ignore obj))
  (let ((prob (loop
		 for tail on (cdr table) by #'cddr
		 when (match-ids (car tail) lastids)
		 return (cadr tail))))
    (unless prob
      (error "Node for ~s has no entry for ~s in ~s."
	     (graph-node-id node) lastids table))
    (next-1 prob)))

(defun match-ids (user last)
  ;; match the user's ids with the past choices.  * is a wildcard.  Matching
  ;; could get really fancy if we wanted it to.
  (cond ((null user) t)
        ((null last) nil)
        ((consp user)
         (and (match-ids (car user) (car last))
              (match-ids (cdr user) (cdr last))))
        ((eq user last) t)
        ((eq user '*) t)
        ((eq last '*) t)
        (t nil)))

(defmethod next-in-pattern ((obj graph))
  (let* ((last (graph-last obj))
         (graph (pattern-data obj))
         (nodes (cdr graph))
         (this (car graph))
         (next nil))
    (setf next
          (funcall (graph-selector obj)
		   obj this (if last (cdr last))))
    (if next
        (let ((node
               (find-if #'(lambda (x) (eq next (graph-node-id x)))
                        nodes)))
          (if node
              (progn
		;; next item becomes selected node. car of last is the number of
		;; choices to remember.  push old selection onto the list and
		;; flush the the oldest element. since we cant setf nthcdrin
		;; some lisps, we setf the cdr of nthcdr-1...
                (rplaca graph node)
                (when last
                  (rplacd last
			  (cons (graph-node-id this) (cdr last)))
                  (rplacd (nthcdr (- (car last) 1) (cdr last))
			  '())))
	      (error "No node for id ~s." next)))
	(error "No next node from ~s." (graph-node-id this)))
    (graph-node-datum this)))

(defmethod map-pattern-data (fn (obj graph))
  (map nil
       #'(lambda (x) (funcall fn (graph-node-datum x)))
       (pattern-data obj)))

;;;
;;; accumulation adds the current item to the set of items
;;; selected so far:  A A B A B C | A A B A B C 
;;;

(progn
  (defclass accumulation (pattern)
    ((indices :initform (cons 0 0) :accessor accumulation-indicies)))
  (defparameter <accumulation> (find-class 'accumulation))
  (finalize-class <accumulation>))

(defmethod next-in-pattern ((obj accumulation))
  (let ((indices (accumulation-indicies obj)))
    (let ((val (elt (pattern-data obj) (car indices))))
      (if (= (car indices) (cdr indices))
          (progn
            (rplaca indices 0)
            (rplacd indices (mod (+ 1 (cdr indices))
				 (pattern-length obj))))
	  (rplaca indices (+ 1 (car indices))))
      val)))

(defmethod map-pattern-data (fn (obj accumulation))
  (map nil fn (pattern-data obj)))

(defmethod default-period-length ((obj accumulation))
  (let ((len (pattern-length obj)))
    (loop for i from 1 to len sum i)))

;;;
;;; thunk calls a function to return the items constituting the data for the
;;; next period.
;;;

(progn
  (defclass thunk (pattern) nil)
  (defparameter <thunk> (find-class 'thunk))
  (finalize-class <thunk>))

(defmethod pattern-external-inits ((obj thunk))
  (append (list ':of (expand-pattern-value (car (pattern-data obj))))
          (call-next-method)))

(defmethod default-period-length ((obj thunk)) obj 1)

;; (defmethod initialize-instance :after ((obj thunk) &rest args)
;;   (let ((data (pattern-data obj)))
;;     (unless (and (consp data) (functionp (car data)))
;;       (error "Thunk not function: ~s." data))
;;     (values)))

(defmethod next-in-pattern ((obj thunk))
  (let ((data (pattern-data obj)))
    (when (null (cdr data))
      (let ((vals (funcall (car data))) (len nil))
        (cond ((null vals) (setf vals (list +nad+)))
              ((not (consp vals)) (setf vals (list vals))))
        (setf len (length vals))
        (rplacd data vals)
        (setf (pattern-length obj) len)
        (when (logtest (pattern-flags obj) +default-period+)
          (let ((p (pattern-period obj)))
            (setf (period-count p) len)
            (setf (period-length p) len)))))
    (pop (cdr data))))

;(setf x (new thunk :of (lambda () (list 1 2 3))))
;(next x t)
;(setf x (new thunk :of (lambda () (list 1 2 3)) :for 5))
;(next x t)

(defmethod map-pattern-data (fn (obj thunk))
  (map nil fn (cdr (pattern-data obj))))

;;;
;;; rotation
;;;

(progn
  (defclass rotation (cycle)
    ((change :initform 0 :initarg :rotations :accessor rotation-change)))
  (defparameter <rotation> (find-class 'rotation))
  (finalize-class <rotation>))

(defmethod pattern-external-inits ((obj rotation))
  (append (list ':of
                (cons 'list
                      (loop
			 for x in (car (pattern-data obj))
			 collect (expand-pattern-value x))))
          (if (equal (rotation-change obj) 0)
              (list)
	      (list ':rotations
		    (expand-pattern-value (rotation-change obj))))))

(defmethod initialize-instance :after ((obj rotation) &rest args)
  ;; pattern is initialized now so that rotations only happen after the first
  ;; cycle
  (let ((data (pattern-data obj)))
    (rplacd data (car data))))

(defmethod next-in-pattern ((obj rotation))
  (let ((ring (pattern-data obj)))
    (when (null (cdr ring))
      (let ((change (next-1 (rotation-change obj)))
            (start nil)
            (step nil)
            (width nil)
            (end nil))
        (if (consp change)
            (progn
              (setf start (pop change))
              (setf step (if (null change) nil (pop change)))
              (setf width (if (null change) nil (pop change)))
              (setf end (if (null change) nil (pop change))))
	    (setf start change))
        (unless start (setf start 0))
        (unless step (setf step 1))
        (unless width (setf width 1))
        (unless end (setf end (- (pattern-length obj) width)))
        (rplacd ring (rotate-items (car ring) start end step width))))
    (pop (cdr ring))))

(defun rotate-items (items start end step width)
  (loop
     for i from start below end by step
     for a = (elt items i)
     for b = (elt items (+ i width))
     do
       (setf (elt items i) b)
       (setf (elt items (+ i width)) a))
  items)

;; (setf a (new rotation of (list 1 2 3 4)))
;; (next a t)

;;;
;;; rewrite
;;;

; (setf a (new rewrite of '((a :-> (b)) (b :-> (b a a)))))

(defstruct rewrite-node datum id to props)

(defstruct rewrite-rule trigger successors context)

(progn
  (defclass rewrite (pattern)
    ((table :initform nil :initarg :initially :accessor rewrite-table)
     (rules :initform '() :initarg :rules :accessor rewrite-rules)
     (generations :initform most-positive-fixnum :initarg :generations :accessor rewrite-generations)))
  (defparameter <rewrite> (find-class 'rewrite))
  (finalize-class <rewrite>))

(defun hash-fold (func prev table)
  (maphash #'(lambda (key val)
               (setf prev (funcall func key val prev)))
           table)
  prev)

(defmethod pattern-external-inits ((obj rewrite))
  (flet ((fnc (n) (cons 'list
			(append (list (expand-pattern-value
				       (rewrite-node-datum n)))
				(if (eq (rewrite-node-datum n) (rewrite-node-id n))
				    (list)
				    (list ':id (rewrite-node-id n)))
				(list ':to
				      `',(loop
					    for x in (rewrite-node-to n)
					    collect (if (listp x)
							(rewrite-node-id
							 x)
							x)))))))
    (append (list ':of
                  (cons 'list
                        (hash-fold
                         #'(lambda (k v l) k (cons (fnc v) l))
                         '()
                         (rewrite-table obj))))
            (call-next-method))))

(defmethod initialize-instance :after ((obj rewrite) &rest args)
  (let ((table (make-hash-table :size 103 :test #'equal))
        (nodes (pattern-data obj))
        (rules (rewrite-rules obj))
        (preset nil))
    ;; :initially initarg uses table slot to avoid additional slot
    (setf preset
          (or (rewrite-table obj)
              (and (first nodes) (list (rewrite-node-id (first nodes))))))
    ;; enter each node in hashtable by id
    (dolist (n nodes)
      (setf (gethash (rewrite-node-id n) table) n))

    ;; as a runtime opimization we prefetch constant TO ids. this saves a call
    ;; to item and a hash lookup each rewrite.  If not constant (a pattern) it's
    ;; stored in the props field, which is otherwise unused by the pattern and
    ;; the ids are looked up each rewrite.

    (dolist (n nodes)
      (let ((x (rewrite-node-to n)))
        (if (pattern? x)
            (progn
              (setf (rewrite-node-props n) (rewrite-node-to n))
              (setf (rewrite-node-to n) nil))
	    (progn
	      (setf (rewrite-node-to n) (lookup-successors x table))))))

    ;; set max generations
    (let ((count (rewrite-generations obj)))
      (setf (rewrite-generations obj)
            (cons 1
                  (if (> count 1)
                      count
		      (error "Generations: ~s not > 1." count)))))

    ;; preset 1st generation.  Defaults to first node.
    (setf nodes
          (loop
	     for id in (if (listp preset) preset (list preset))
	     collect (or (gethash id table)
			 (error "Id ~s not in rewrite nodes." id))))
    (setf (rewrite-table obj) table)
    (setf (pattern-data obj) (cons nodes nodes))
    (unless (null rules)
      (setf (rewrite-rules obj) (parse-rules rules table)))
    (values)))

(defmethod canonicalize-pattern-data ((obj rewrite) data parser inits)
  (flet ((parse-rewrite-node (extern)
	   (let ((datum nil) (keys '()))
	     (if (consp extern)
		 (progn
		   (setf datum (car extern))
		   (setf keys (cdr extern)))
		 (setf datum extern))
	     (loop
		with args = '() and id and key and val
		until (null keys)
		do
		  (setf key (pop keys))
		  (setf val
			(if (null keys)
			    (error "Not a rewrite spec: ~S." extern)
			    (pop keys)))
		  (push val args)
		  (case key
		    ((id :id) (setf id val) (push ':id args))
		    ((to -> :to :->) (push ':to args))
		    (t (error "~s not one of rewrite keywords: id ->." key)))
		finally (progn (unless id
				 (push datum args)
				 (push ':id args))
			       (return (apply #'make-rewrite-node :datum (maybeparse parser datum) args)))))))
    (let ((intern (mapcar #'parse-rewrite-node data)))
      (values intern
              (length intern)
              (not (some #'(lambda (x) (pattern? (rewrite-node-datum x)))
			 intern))))))

(defmethod map-pattern-data (fn (obj rewrite))
  (map nil
       #'(lambda (x) (funcall fn (rewrite-node-datum x)))
       (car (pattern-data obj))))

(defun parse-rules (rules table)
  (labels ((getnode (id table rule)
             (or (gethash id table)
                 (if (eq id '*)
                     '*
		     (error "No node for id ~s in rule ~s." id rule))))
           (getnodes (ids table rule)
             (loop for id in ids collect (getnode id table rule))))
    (loop
       for rule in rules
       collect (loop
		  with form = rule and left = t and x
		  while (not (null form))
		  do (setf x (pop form))
		  if (or (eq x '->) (eq x ':->))
		  do (setf left nil)
		  else if left collect x into lh
		  else collect x into rh
		  finally
		    (return (let ((len (length lh)))
			      (unless (not left)
				(error "Missing -> in rule ~s." rule))
			      (cond ((= len 0)
				     (error "Missing left hand side in ~s." rule))
				    ((= len 1)
				     (make-rewrite-rule
				      :successors (getnodes rh table rule)
				      :trigger (getnode (if (consp (car lh))
							    (caar lh)
							    (car lh))
							table
							rule)))
				    (t (loop
					  for tail on lh
					  for x = (car tail)
					  for i from 0
					  when (consp x)
					  do
					    (rplaca tail (car x))
					    (return (make-rewrite-rule
						     :successors (getnodes rh table rule)
						     :trigger (getnode (car x) table rule)
						     :context (cons (cons i len) (getnodes lh table rule))))
					  finally (error "No trigger in lh side of ~s." rule))))))))))

(defun lookup-successors (successor table)
  ;; get successor nodes. successor may be an id, list of ids or nil.
  (if (consp successor)
      (loop
	 for to in successor
	 collect (or (gethash to table)
		     (error "No rewrite node for id ~s." to)))
      (if successor
	  (list (or (gethash successor table)
		    (error "No rewrite node for ~s." successor)))
	  nil)))

(defmethod next-in-pattern ((obj rewrite))
  (let ((nodes (pattern-data obj)))
    (when (null (cdr nodes))
      (let ((count (rewrite-generations obj)))
        (if (< (car count) (cdr count))
            (rewrite-generation obj t nil)
	    (rplacd nodes (car nodes)))))
    (rewrite-node-datum (pop (cdr nodes)))))

; (setf a (new rewrite of '((a -> (b)) (b -> (b a a)))))
; (next a 20)

(defun rewrite-generation (obj &optional (new nil) (ids t))
  (let ((data (pattern-data obj)))
    (if (not new)
        (if (not ids)
            (car data)
	    (mapcar #'rewrite-node-datum (car data)))
	(let* ((nodes (pattern-data obj))
	       (rules (rewrite-rules obj))
	       (old (car nodes))
	       (new (if (null rules)
			(node-rewrite old (rewrite-table obj))
			(rule-rewrite old rules))))
	  (when (null new)
	    (error "Rewrite generation #~s is empty!" (+ (car (rewrite-generations obj)) 1)))
	  (progn (rplacd nodes new) (rplaca nodes new))
	  (if (not ids)
	      (car nodes)
	      (mapcar #'rewrite-node-datum (car data)))))))

(defun node-rewrite (gen table)
  ;; nodes specify their rewrites directly just fetch successors
  (loop
     for node in gen
     for next = (let ((to (rewrite-node-to node)))
		  (if to
		      (copy-list to)
		      (lookup-successors
		       (next-1 (rewrite-node-props node))
		       table)))
     nconc next))

(defun generation-mismatch (test seq gen beg end)
  (let ((start1 0) (end1 (length seq)) (start2 beg) (end2 end))
    (do ((i1 start1 (+ 1 i1))
         (i2 start2 (+ 1 i2))
         (x1 nil)
         (x2 nil)
         (done nil))
        ((or done (and (>= i1 end1) (>= i2 end2)))
         (if done done nil))
      (if (>= i1 end1)
          (setf done i1)
	  (if (>= i2 end2)
	      (setf done i1)
	      (progn
		(setf x1 (elt seq i1))
		(setf x2 (elt gen i2))
		(if (funcall test x1 x2) nil (setf done i1))))))))

(defun rule-rewrite (generation rules)
  (loop
     with context and len
     for index from 0
     for node in generation
     append (loop
	       for rule in rules
	       do (when (eq node (rewrite-rule-trigger rule))
		    (setf context (rewrite-rule-context rule))
		    (if (not (null context))
			(let* ((seq (cdr context))
			       (beg (- index (caar context)))
			       (end (+ beg (cdar context))))
			  (when (and (<= 0 beg end (or len (setf len (length generation))))
				     (not (generation-mismatch #'(lambda (a b) (or (eq a '*) (eq a b)))
							       seq generation beg end)))
			    (return (rewrite-rule-successors rule))))
			(return (rewrite-rule-successors rule))))
	       finally (return (list)))))


;;;
;;; RANGE
;;;

(defun %range-stepping? (flags) (logtest flags +range-stepping+))
(defun %range-unbounded? (flags) (logtest flags +range-unbounded+))
(defun %range-initially? (flags) (logtest flags +range-initially+))
(defun %range-dynamic? (flags) (logtest flags +range-dynamic+))
(defun %range-random? (flags) (logtest flags +range-random+))

(progn
  (defclass range (pattern)
    ((from :initform nil :initarg :from :initarg :initially :accessor range-from)
     (to :initform nil :initarg :to :initarg :below :initarg :pickto :accessor range-to)
     (downto :initform nil :initarg :downto :initarg :downto :accessor range-downto)
     (above :initform nil :initarg :above :initarg :above)
     (below :initform nil :initarg :below :initarg :below)
     (by :initform nil :initarg :by :initarg :stepping :accessor range-by)
     (incf :initform nil :accessor range-incf)
     (test :initform nil :accessor range-test)))
  (defparameter <range> (find-class 'range))
  (finalize-class <range>))

(defmethod pattern-external-inits ((obj range))
  ;; from to downto above below by pickto
  (let ((bits (pattern-flags obj)))
    (append (list (if (%range-initially? bits) ':initially ':from)
                  (expand-pattern-value (range-from obj)))
            (if (range-to obj)
                (list (if (%range-random? bits)
			  ':pickto
			  (if (funcall (range-test obj) 2 0 2) ':below ':to))
                      (expand-pattern-value (range-to obj)))
		(if (range-downto obj)
		    (list (if (funcall (range-test obj) 0 0 2) ':above :downto)
			  (expand-pattern-value (range-downto obj)))
		    (list)))
            (list (if (%range-stepping? bits) ':stepping ':by)
                  (expand-pattern-value (range-by obj)))
            (call-next-method))))

(defmethod initialize-instance :after ((obj range) &rest args)
  (let* ((raw (pattern-data obj))
         (data (car raw))
         (init (cadr raw))
         flag
	 test
	 bits)

    ;; data (inc from val min max) 
    ;; init (<from> <to> <down> <by> <incf> <flags> <bits>))

    (setf (pattern-data obj) data)
    (setf (range-from obj) (elt init 0))
    (setf (range-to obj) (elt init 1))
    (setf (range-downto obj) (elt init 2))
    (setf (range-by obj) (elt init 3))
    (setf (range-incf obj) (elt init 4))
    (setf flag (elt init 5))
    (setf bits (elt init 6))
    (case flag
      ((#b00001)					    ;to
       (setf test #'(lambda (x min max) min (> x max))))
      ((#b00010)					    ;below
       (setf test #'(lambda (x min max) min (>= x max))))
      ((#b00100)					    ;downto
       (setf test #'(lambda (x min max) max (< x min))))
      ((#b01000)					    ;above
       (setf test #'(lambda (x min max) max (<= x min))))
      ((#b00101)					    ;downto & to
       (setf test #'(lambda (x min max) (or (< x min) (> x max)))))
      ((#b00110)					    ;downto & >below
       (setf test #'(lambda (x min max) (or (< x min) (>= x max)))))
      ((#b01001)					    ; above & to
       (setf test #'(lambda (x min max) (or (<= x min) (> x max)))))
      ((#b01010)					    ; above<->below
       (setf test #'(lambda (x min max) (or (<= x min) (>= x max)))))
      ((#b10000)					    ; within
       nil)
      ((0)
       (setf bits (logior bits +range-unbounded+)))
      (t
       (error "Not a range specification: ~s." args)))
    
    (when (and (%range-initially? bits)
               (not (%range-unbounded? bits)))
      (error ":initially excludes lower or upper bound."))
    
    ;; if bounds were specified without explicit period then range period is
    ;; dynamic
    (when (eq (period-length (pattern-period obj))
              most-positive-fixnum)
      (setf bits (logior bits +range-dynamic+)))

    ;; add in range bit flags
    (setf (pattern-flags obj) (logior (pattern-flags obj) bits))
    (setf (range-test obj) test)))

(defun between (lb ub &optional exception (state *random-state*))
  (let ((range (- ub lb)))
    (if (not (> range 0))
        lb
	(if exception
	    (loop for num = (+ lb (random range state))
	       when (not (= exception num)) return num)
	    (+ lb (random range state))))))

  ;; this is called BEFORE range's initialize-instance method

(defmethod canonicalize-pattern-data ((obj range) data parser inits)
  (let ((from 0)
        (to nil)
        (downto nil)
        (by 1)
        (incf #'+)
        (flag 0)
        (bits 0)
        (const? t))
    (dopairs (a v inits)
      (case a
	((:from) (setf from v))
	((:initially)
	 (setf from v)
	 (setf bits (logior bits +range-initially+)))
	((:to)
	 (setf to v)
	 (setf flag (logior flag 1))
	 (setf incf #'+))
	((:below)
	 (setf to v)
	 (setf flag (logior flag 2))
	 (setf incf #'+))
	((:downto)
	 (setf downto v)
	 (setf flag (logior flag 4))
	 (setf incf #'-))
	((:above)
	 (setf downto v)
	 (setf flag (logior flag 8))
	 (setf incf #'-))
	((:pickto)
	 (setf to v)
	 (setf bits (logior bits +range-random+))
	 (setf flag (logior flag 16))
	 (setf incf #'between))
	((:by)
	 (setf by v))
	((:stepping)
	 (setf by v)
	 (setf bits (logior bits +range-stepping+)))))
    
    (if (or (pattern? from)
            (pattern? to)
            (pattern? downto)
            (pattern? by))
        (setf const? nil))

    ;; data ((inc from val min max) 
    ;;       (<from> <to> <down> <by> <flags> <bits> <incf>))
    ;; initialize-instance sets inc to NIL if stepping 
    (values (if const?
                (list (list by from nil downto to)
                      (list from to downto by incf flag bits))
		(list (list nil nil nil nil nil)
		      (list from to downto by incf flag bits)))
            (if (or to downto) most-positive-fixnum 1)
            const?)))

(defun reset-range? (data test)
  ;; data is (inc from val min max)
  (or (not (third data))				    ;no current FROM
      (and test (apply test (cddr data)))))

(defmethod next-in-pattern ((obj range))
  (let ((bits (pattern-flags obj))
        (data (pattern-data obj))
        (test (range-test obj))
        (from nil))
    (when (reset-range? data test)
      (if (logtest bits +constant-data+)
	  ;; data is (inc from val min max)
	  (progn
            (unless (and (%range-initially? bits) (third data))
              (rplaca (cddr data) (cadr data))))
	  (progn
	    (unless (%range-stepping? bits)
	      ;; set first element
	      (rplaca data (next-1 (range-by obj))))
	    ;; set second element
	    (rplaca (cdr data) (next-1 (range-from obj)))
	    (unless (and (%range-initially? bits) (third data))
	      ;; set third element
	      (rplaca (cddr data) (cadr data)))
	    ;; set fourth
	    (rplaca (cdddr data) (next-1 (range-downto obj)))
	    ;; set fifth
	    (rplaca (cddddr data) (next-1 (range-to obj))))))
    
    (if (%range-random? bits)
        (progn
          (setf from
                (funcall (range-incf obj)
                         (second data)
                         (car (cddddr data))))		    ;5th elt is TO
          (rplaca (cddr data) from))
	(progn
	  (setf from (third data))
	  (rplaca (cddr data)
		  (funcall (range-incf obj)
			   from
			   (if (%range-stepping? bits)
			       (next-1 (range-by obj))
			       (first data))))))

    ;; signal EOP if dynamic and range will reset next time
    (if (and (%range-dynamic? bits) (reset-range? data test))
        (setf (period-count (pattern-period obj)) 1))
    from))

(defmethod reset-period ((obj range))
  (let ((val (call-next-method)))
    ;; reset the bounds if not dynamic
    (let ((bits (pattern-flags obj)))
      (if (and (%range-unbounded? bits)
               (not (%range-initially? bits)))
          (rplaca (cddr (pattern-data obj)) nil)))
    val))


;;;
;;; pval holds a lisp form to be evaluated in a pattern.
;;; (pval x)
;;;

(defmacro pval (expr) `(lambda () ,expr))

;(define-class* <pval> ()
;  ((thunk :init-keyword :of :accessor pval-thunk))
;  :name 'pval)

;; (defclass <pval> ()
;;   ((thunk :initarg :of :accessor pval-thunk)))

;(define-method* (make-load-form (obj <pval>))
;  `(pval ,(pval-thunk obj)))

;; (defmethod pattern? ((obj <pval>)) obj)

;; (define-macro (pval expr)
;;   `(lambda () ,expr)
;;   ;;`(make <pval> :of (lambda () ,expr))
;;   )

;(define-method* (next-1 (obj <pval>))
;  ( (pval-thunk obj) )) ; funcall

;;;
;;; Join (defmultiple-item)
;;;

(progn
  (defclass join (pattern)
    ((format :accessor join-format :initarg :format :initform nil)
     (cache :accessor join-cache :initform nil)))
  (defparameter <join> (find-class 'join))
  (finalize-class <join>))

(defmethod pattern-external-inits ((obj join))
  (append (list ':of
                (cons 'list
                      (loop
			 for x in (pattern-data obj)
			 collect (expand-pattern-value x))))
          (list ':format (expand-pattern-value (join-format obj)))
          (call-next-method)))

(defmethod initialize-instance :after ((obj join) &rest args)
  (let* ((per (pattern-period obj))
         (len (length (pattern-data obj)))
         (data (pattern-data obj))
         (fmat (join-format obj))
         (ppat nil))
    (setf (period-stream per) (period-default per))
    (cond ((not fmat)
           (setf fmat (make-list len :initial-element ':eop))
           (setf (join-format obj) fmat))
          ((not (listp fmat))
           (error "Expected :format list but got ~s instead." fmat))
          ((not (= (length fmat) len))
           (error ":format list has ~s elements but length of data is ~s."
		  (length fmat) len)))

    ;; if user specified a format list each element is:
    ;;   :eop - datum marks eop
    ;;   t   - datum read each time
    ;;   nil   - datum read each period
    ;; set each element in format to either:
    ;;   :eop  - datum is pattern and sets eop
    ;;   :each - datum is pattern read each time
    ;;   :once - datum is pattern read each period
    ;;   nil    - datum is not pattern (next-1 never called)

    (dotimes (i len)
      (case (elt fmat i)
        ((:eop)
	 (if (pattern? (elt data i)) nil (setf (elt fmat i) nil)))
        ((t)
	 ;; user says read each time
	 (if (pattern? (elt data i))
		 (setf (elt fmat i) ':each)
		 (setf (elt fmat i) nil)))
        ((nil)
         ;; user says read each period
	 (if (pattern? (elt data i))
	     (progn (setf ppat t) (setf (elt fmat i) ':once))
	     (setf (elt fmat i) nil)))
        (t (error "Bad :format value ~s: not :eop, ~s or ~s"
		  (elt fmat i) t nil))))
    (setf (join-format obj) fmat)

    (when ppat
      ;; cache patterns read once per period
      ;; elements either nil or pattern to read
      (setf ppat (make-list len :initial-element nil))
      (dotimes (i len)
        (if (eq (elt fmat i) ':once)
            (progn
              (setf (elt ppat i) (elt data i))
              (setf (elt data i) nil))))
      (setf (join-cache obj) ppat))
    (values)))

(defmethod canonicalize-pattern-data ((obj join) data parser inits)
  (let ((subs (not (some #'pattern? data))))
    (values data (if subs 1 most-positive-fixnum) (not subs))))

(defmethod reset-period ((obj join))
  ;; update data from patterns that get read once per period.
  (let ((val (call-next-method)))
    (let ((c (join-cache obj)))
      (when c
        ;; read patterns marked once per period each element in c
        ;; either nil or pattern
        (let ((d (pattern-data obj)) (i 0))
          (dolist (x c)
            (if x (setf (elt d i) (next-1 x)))
            (setf i (+ i 1))))))
    val))

(defun join-eop? (data fmat)
  ;; join at eop if every pattern marked :eop is at eop.
  (do ((end? t))
      ((null data) end?)
    (if (eq (car fmat) ':eop)
        (if (eop? (car data)) nil (setf end? nil)))
    (setf data (cdr data))
    (setf fmat (cdr fmat))))

(defmethod next-in-pattern ((obj join))
  (let ((next (list nil))
        (data (pattern-data obj))
        (fmat (join-format obj)))
    (do ((l1 data (cdr l1)) (l2 fmat (cdr l2)) (l3 next (cdr l3)))
        ((null l1) nil)
      (if (not (car l2))
          (rplacd l3 (list (car l1)))
	  (rplacd l3 (list (next-1 (car l1))))))
    (when (join-eop? data fmat)
      (setf (period-count (pattern-period obj)) 1))
    (cdr next)))

;; (setf x (new join of (list (new cycle of '(a b)) 2)))

;; (setf x (new cycle 
;;           of (list (new join of (list (new cycle of '(a b)) 1))
;;                    (new join of (list (new cycle of '(c d)) 2)))))
;; (next x)


;;;
;;; copier return multiple copies of a pattern's periods
;;;

(progn
  (defclass copier (pattern)
    ((source :accessor copier-source)
     (repfor :accessor copier-repfor :initform nil :initarg :repeat-for)))
  (defparameter <copier> (find-class 'copier))
  (finalize-class <copier>))

(defmethod pattern-external-inits ((obj copier))
  (append (list ':of (expand-pattern-value (copier-source obj)))
          (call-next-method)))

(defmethod initialize-instance :after ((obj copier) &rest args)
  (let ((data (pattern-data obj)))
    (setf (copier-source obj) (car data))
    (setf (pattern-data obj) (list))
    (values)))

(defmethod next-in-pattern ((obj copier))
  (let ((data (pattern-data obj)))
    (if (null data)
        (let* ((per (pattern-period obj))
               (res (next (copier-source obj) t))
               (len (length res))
               (for (period-length per))
               (rep (copier-repfor obj)))
          (if rep
              (progn
                (setf for (next rep))
                (setf (period-length per) len)
                (setf (period-count per) len))
	      (setf (period-count per) (* len for)))
          (let ((sav res))
            (dotimes (i (- for 1))
              (setf res (append res (copy-list sav)))))
          (setf (pattern-data obj) (cdr res))
          (car res))
	(progn (setf (pattern-data obj) (cdr data)) (car data)))))


;; (setq x (new copier :of (new cycle :of '(a b c) :for 2)  :for 3))
;; (next x t)
;; (setq x (new copier :of (new cycle :of '(a b c) :for 2) :repeat-for 3))
;; (next x t)
;; (setq x (new copier :of (new cycle :of '(a b c) :for 2)
;;             :for (new cycle :of '(2 3))))
;; (next x t)
;; (setq x (new copier :of (new cycle :of '(a b c) :for 2)
;;             :repeat-for (new cycle :of '(2 3))))
;; (next x t)
;; (setf x (new copier :of (list (new cycle :of '(a b))) :for 2))
;; (setf x (new copier :of (list (new cycle :of '(a b))) :for (new cycle :of '(2 3))))
;; (describe x)
;; (next x t)

;;;
;;; the pattern macro
;;;


(defparameter pattern-types '(cycle heap weighting line
			      palindrome graph markov
			      rewrite range rotation))

(defparameter pattern-item-types '())

(defparameter pattern-options '((pattern alias with)
                                (palindrome elide)
                                (heap state)
                                (weighting state adjustable)
                                (markov past produce)
                                (graph last selector props starting-node-index)
                                (rotation rotations)
                                (rewrite initially rules generations)
                                (rotation rotations)
                                (range from initially to below pickto downto above by stepping)
                                repeat eop-hook for name))

(defparameter pattern-no-items (quote (range transposer)))

(defun gather-substs (forms)
  ;; gather with|alias clauses until no more
  (flet ((subst-clause? (form last)
           (and (or (member (car form) '(alias with))
                    (and last (eq (car form) 'and))))))
    ;; tail is (subst <var> = <form> ...)
    ;; or (with <var> = <form> ...)
    (let ((subst (list)))
      (do ((tail forms (cddddr tail)) (last nil))
          ((or (null tail) (not (subst-clause? tail last)))
           (values (reverse subst) tail))
        (setf subst
              (cons (make-pattern-binding
                     (if (eq (first tail) 'and) last (first tail))
                     tail)
                    subst))
        (if (not (eq (car tail) 'and)) (setf last (car tail)))))))

(defun make-pattern-binding (typ args)
  (if (not typ)
      (error "Extraneous 'and' found: ~s" args)
      (if (not (and (cdr args) (cddr args) (cdddr args)))
	  (error "Pattern ended without expected token.")
	  (if (let ((t12 (second args)))
		(and t12 (symbolp t12)))
	      (if (eq (third args) '=)
		  (let ((var (second args)) (val (fourth args)))
		    (if (numberp val)
			(list var val)
			(if (eq typ 'with)
			    (list var val)
			    (list var `(lambda () ,val)))))
		  (error "Expected '=' but got '~s' instead: ~s" (third args) args))
	      (error "Expected variable name (symbol) but got '~s' instead: ~s" (second args) args)))))

; (gather-substs '(and a = 2 ))
; (gather-substs '(alias a = 2 alias b = 3 4 5 6))
; (gather-substs '(alias a = 2 and x = 4  alias b = ziz 4 5 6))
; (gather-substs '(alias a = fof and b = q2 with c = 3 alias x = 4 p q r))
; (gather-substs '(alias a = cycle of  '(2 3)))
; (gather-substs '(alias 1 = 2 alias b = 3 4 5 6))


(defun check-pattern-option (form option err?)
  (cond ((null form) (when err?
		       (error "Pattern ended without expected token.") nil))
        ((consp option) (if (member (car form) option)
			    t
			    (when err?
			      (error "Expected pattern option from ~S but got ~S instead." option form))))
        (t (if (eq (car form) option)
	       t
	       (when err?
		 (error "Expected pattern option ~S but got ~S instead." option form))))))

(defun get-pattern-options (ptyp etyp)
  (do ((tail pattern-options (cdr tail))
       (opts '()))
      ((null tail) opts)
    (if (consp (car tail))
        (if (or (eq (caar tail) ptyp) (eq (caar tail) etyp))
            (setf opts (append opts (cdr (car tail)))))
	(setf opts (cons (car tail) opts)))))

; (get-pattern-options 'palindrome nil)
; (get-pattern-options 'palindrome t)


(defun expand-pattern-item (item ptyp etyp vars)
  (cond ((null item) '(list))
        ((consp item) (if (not (member ptyp '(weighting markov graph rewrite)))
			  `',item
			  `(cons ,(expand-pattern-item (car item) nil etyp vars)
				 ,(expand-pattern-item (cdr item) ptyp etyp vars))))
        ((keywordp item) item)
        ((and item (symbolp item)) (if (assoc item vars) item `',item))
        ((numberp item) item)
        (t `',item)))

(defun gather-pattern-data (args num stop ptyp etyp vars)
  (do ((tail args) (gets (list)) (done nil) (nget 0))
      ((or (null tail) done)
       (cond ((null args)
	      (error "Pattern ended without pattern data."))
	     ((or (and (numberp num) (< nget num))
		  (and (eq num t) (null gets)))
	      (error "Expected pattern data but got ~S instead."
		     args))
	     (t (values (cons 'list (nreverse gets)) tail))))
    (if (or (member (car tail) stop)
            (and (numberp num) (= nget num)))
        (setf done t)
	(let ((x (expand-pattern-item (car tail) ptyp etyp vars)))
	  (setf gets (cons x gets))
	  (setf nget (+ nget 1))
	  (setf tail (cdr tail))))))

; (expand-pattern-item 'abc 'cycle nil '())
; (expand-pattern-item 'abc 'weighting nil '((abc 1) (xyz 2)))
; (expand-pattern-item 'foo 'weighting nil '((abc 1) (xyz 2)))
; (expand-pattern-item '(foo :weight 33) 'weighting nil '((abc 1) (xyz 2)))
; (expand-pattern-item 'abc 'cycle nil '())
; (expand-pattern-item 'abc 'cycle nil '()) 

; (gather-pattern-data '(1 2 3 4 for 4) nil '(for) 'cycle nil ())
; (gather-pattern-data '(a b c d e for 4) nil '(for) 'cycle nil '((c 11)))
; (gather-pattern-data '() nil '(for))
; (gather-pattern-data '(for 2) nil '(for))



(defun expand-pattern-macro (form)
  (let ((vars (list))
        (args (list))
        (init (list))
        (ptyp nil)					    ;pattern type
        (etyp nil)					    ;element type
        (elts nil)
        (opts nil))
    (multiple-value-setq (vars args) (gather-substs form))
    (check-pattern-option args pattern-types t)
    (setf ptyp (pop args))
    ;; parse items if pattern takes them
    (cond ((member ptyp pattern-no-items)
	   (setf opts (get-pattern-options ptyp nil)))
          (t
	   ;; parse optional element type
	   (if (check-pattern-option args 'of nil)
	       (progn
		 (pop args)
		 (check-pattern-option args pattern-item-types t)
		 (setf etyp (pop args))))
	   (setf opts (get-pattern-options ptyp etyp))
	   (multiple-value-setq (elts args)
	     (gather-pattern-data args t opts ptyp etyp vars))
	   (setf etyp (if (not etyp) ':of (symbol->keyword etyp)))
	   (setf init (list etyp elts))))

    ;; process remaining pairwise args
    (do ((tail args)
	 (optn nil)
	 (valu nil)
	 (seen (list)))
        ((null tail) nil)
      (check-pattern-option tail opts t)
      (setf optn (pop tail))
      (if (member optn seen)
          (error "Found duplicate option ~A in ~S." optn args)
	  (push optn seen))
      (setf valu (pop tail))
      (setf init (nconc init (list (symbol->keyword optn) valu))))
    (setf form `(make-instance (find-class ',ptyp) ,@init))
    (if (null vars)
	form
	(list 'let* vars form))))

(defmacro pattern (&body args) (expand-pattern-macro args))

; (defmacro defpattern ((name &rest args) &body)
;   (let ((user (loop for x in args nconc (list (gentemp) a))))
;     `(defmacro ,name ,(loop for a in user collect (car a))
;        (pattern ,@ (loop for a in user nconc (list (second a) '= (first a)))
; 	   ,@body))))
;(defmacro foo (v11 v232 v333)
;  `(pattern subst a = ,v11
;	    subst b = ,v232
;(pattern alias foo = (pattern cycle 1 2 3 4 5)
;	 alias bar = (pattern heap 1 2 3 4 5)
;	 cycle 1 2 3 4 (@ foo) (@ bar)
;	 )

