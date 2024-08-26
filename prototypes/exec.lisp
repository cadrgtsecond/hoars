;;; Lisp code implementing the execution model of the language

;;; A word looks like this
;;; (<function> <var1> <var2> <var3>)

;;; A call looks like this
;;; (<closure> <arg1> <arg2> <arg3>)

(defparameter *stack* '())

(defun call-in (in)
  (funcall (caar in) in))

(defun hello-world (rest)
  (format t "~a-------~a~%" rest *stack*))

(defun push-val (rest)
  (format t "Pushing ~a~%" (cadr rest))
  (push (cadr rest) *stack*)
  (call-in (cddr rest)))

(defun add (rest)
  (format t "Adding ~a~%" *stack*)
  (setf *stack* (cons (+ (car *stack*) (cadr *stack*)) (cddr *stack*)))
  (call-in (cdr rest)))

(defun docol (rest)
  (call-in (cdar rest))
  (call-in (cdr rest)))
(defun ret (rest))

(setf *stack* '())
(call-in `((docol (push-val) 10 (push-val) 20 (add) (ret)) (hello-world)))

