;;;; Prototype compiler

(ql:quickload :str)

(defparameter *input* "")

(defun read-word ()
  "Reads a token from the input, moving it forward. Returns NIL if no more output"
  (destructuring-bind (&optional res rest) (str:split " " *input* :limit 2 :omit-nulls t)
    (setf *input* rest)
    res))
(defun read-string-quote ()
  "Reads a string literal from the input"
  (when (equal (str:s-first *input*) "\"")
    "string-quote"))
(defun read-token ()
  (or (read-string-quote) (read-word)))

(defun peek-token ()
  "Peeks a token without consuming it"
  (let ((*input* *input*))
    (read-token)))

(defparameter *-compiler-* (make-hash-table :test #'equal))
;;(defparameter *-variables-* (make-hash-table :test #'equal))
(defparameter *-values-* (make-hash-table :test #'equal))
(define-symbol-macro *current-word* (gethash "current-word" *-values-*))

(defmacro defword (name dict arglist &body code)
  `(setf (gethash ,name ,dict)
     (list (lambda ,arglist ,@code))))

(defparameter *stack* '())
(defun push-val (self)
  (format t "Pushing ~a~%" (cadr self))
  (push (cadr self) *stack*)
  (call-in (cddr self)))
(defun debug-log (self)
  (format t "~a------------~a" self *stack*)
  (call-in (cdr self)))

(defword "quote" *-compiler-* (self)
  (list '(push-val) (read-token)))
(defword "string-quote" *-compiler-* (self)
  (print *input*))
(defword "trace" *-compiler-* (self)
  (list '(debug-log)))

(defun rlet (self)
  (setf (gethash (cadr self) *-values-*) (car *stack*))
  (let ((*stack* (cdr *stack*)))
    (call-in (cddr self))))
(defun rpush-var (self)
  (push (gethash (cadr self) *-values-*) *stack*)
  (call-in (cddr self)))
(defword "let" *-compiler-* (self)
  (let ((name (read-word))
        (sep (read-token)))
    (when (string= sep "=")
      (defword name *-compiler-* (self)
        `((rpush-var) ,name))
      `(,@(compile-expr) (rlet) ,name ,@(compile-expr)))))

(defun call-in (in)
  (and in (funcall (caar in) in)))

(defun compile-expr ()
  "Compiles an expression from the input"
  (call-in (list (gethash (read-token) *-compiler-*))))

#+nil
(let ((*input* "let x = quote 20 trace"))
  (compile-expr))
