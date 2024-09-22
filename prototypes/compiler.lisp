;;;; Prototype compiler

(ql:quickload :str)
(ql:quickload :alexandria)
(defpackage #:hoars
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)))
(in-package #:hoars)

(defparameter *input* "")

(defun read-word ()
  "Reads a token from the input, moving it forward. Returns NIL if no more output"
  (destructuring-bind (&optional res rest) (str:split "\\s+" *input* :limit 2 :omit-nulls t :regex t)
    (setf *input* rest)
    res))
(defun read-string-quote ()
  "Reads a string literal from the input"
  (when (or (equal (str:s-first *input*) "\"") (equal (str:s-first *input*) "'"))
    "string-quote"))
(defun read-token ()
  (or (read-string-quote) (read-word)))

(defun peek-token ()
  "Peeks a token without consuming it"
  (let ((*input* *input*))
    (read-token)))

;;(defparameter *-variables-* (make-hash-table :test #'equal))
(defparameter *-values-* (make-hash-table :test #'equal))

(define-symbol-macro *-compiler-* (gethash "-compiler-" *-values-*))
(setf *-compiler-* (make-hash-table :test #'equal))

(defmacro defword (name dict arglist &body code)
  `(setf (gethash ,name ,dict)
     (list (lambda ,arglist ,@code))))

(defparameter *stack* '())
(defun push-val (self)
  (push (cadr self) *stack*)
  (call-in (cddr self)))
(defun debug-log (self)
  (format t "~a------------~a" self *stack*)
  (call-in (cdr self)))
(defmacro with-push (values &body body)
  "Pushes VALUES onto the stack in the reverse order specified"
  `(let ((*stack* (list* ,@values *stack*)))
     ,@body))
(defmacro with-pop (vars &body body)
  "Pops VALUES from the stack in the order specified"
  `(destructuring-bind (,@vars &rest *stack*) *stack*
     ,@body))

(defun call-in (in)
  (and in (funcall (caar in) in)))

(defun compile-compiler (word)
  "compiles a *-COMPILER-* word"
  (call-in (list (or (gethash word *-compiler-*) (error "Unknown word")))))
(defun compile-expr ()
  "Compiles an expression from the input"
  (let ((token (read-token)))
    (and token (compile-compiler token))))
(defun wcompile-expr (self)
  (with-push ((compile-expr))
    (call-in (cdr self))))

(defword "quote" *-compiler-* (self)
  (list '(push-val) (read-token)))
(defword "undefined" *-compiler-* (self)
  (list '(push-val) nil))
(defword "string-quote" *-compiler-* (self)
  (print *input*))
(defword "trace" *-compiler-* (self)
  (list '(debug-log)))
(defword "compile-expr" *-compiler-* (self)
  (list '(wcompile-expr)))

(defun rpush-var (self)
  (with-push ((gethash (cadr self) *-values-*))
    (call-in (cddr self))))

(defun print-value (self)
  (format t "=> ~a~%" (car *stack*))
  (call-in (cdr self)))
(defword "print" *-compiler-* (self)
  `(,@(compile-expr) (print-value)))

(defun rlet (self)
  (with-pop (val)
    (setf (gethash (cadr self) *-values-*) val)
    (call-in (cddr self))))
(defword "let" *-compiler-* (self)
  (let ((name (read-word))
        (sep (read-token)))
    (when (string= sep "=")
      (defword name *-compiler-* (self)
        `((rpush-var) ,name))
      `(,@(compile-expr) (rlet) ,name ,@(compile-expr)))))

;;;; Now that we have a way to define variables, let us define `-compiler-` properly as a variable
(let ((*input* "let -compiler- = undefined"))
  ;; Since we do not execute the expression, the value of -compiler- is never changed
  (compile-expr))

(defun docode (self)
  (call-in (cdar self)))
(defword "word" *-compiler-* (self)
  (let ((name (read-word))
        (code (loop until (string= (peek-token) "end")
                    appending (compile-expr)
                    finally (read-token))))
    (setf (gethash name *-compiler-*) `(docode ,@code))
    (compile-expr)))

(defun jump (self)
  "Unconditionally jumps to (cadr self)"
  (call-in (cadr self)))
(defun cjump (self)
  "Checks to condition on top of the stack and jumps to (cadr self) if it is true"
  (with-pop (condition)
    (if condition
        (call-in (cadr self))
        (call-in (cddr self)))))
(defun cnjump (self)
  "Checks to condition on top of the stack and jumps to (cadr self) if it is false"
  (with-pop (condition)
    (if condition
        (call-in (cddr self))
        (call-in (cadr self)))))

(let ((*input* "let true = undefined
                let false = undefined"))
  (call-in (compile-expr)))
(setf (gethash "true" *-values-*) t)
(setf (gethash "false" *-values-*) nil)

(defword "if" *-compiler-* (self)
  (let ((condition (compile-expr))
        (then-body (loop until (or (string= (peek-token) "end") (string= (peek-token) "else"))
                         appending (compile-expr))))
    (if (string= (read-token) "end")
      (let ((rest (compile-expr)))
        `(,@condition (cnjump) ,rest ,@then-body ,@rest))
      (let ((else-body (loop until (string= (peek-token) "end")
                             appending (compile-expr)
                             finally (read-token)))
            (rest (compile-expr)))
        `(,@condition (cnjump) (,@else-body ,@rest) ,@then-body ,@rest)))))

;;; String
(defword "string-quote" *-compiler-* (self)
  (destructuring-bind (val &optional rest)
      (str:split "['\"]" (subseq *input* 1) :omit-nulls t :limit 2 :regex t)
    (setf *input* (str:trim-left rest))
    (list '(push-val) val)))

(defun wget (self)
  (with-pop (object index)
    ;; TODO: Implement other types of objects
    (with-push ((gethash index object))
      (call-in (cdr self)))))
;; get object index
(defword "get" *-compiler-* (self)
  `(,@(compile-expr) ,@(compile-expr) (wget)))

(defun wmake-table (self)
  (with-push ((make-hash-table :test #'equal))
    (call-in (cdr self))))
(defword "make-table" *-compiler-* (self)
  `((wmake-table)))

#+nil
(let ((*input* "print get '-compiler-' -compiler-"))
  (compile-expr))

#+nil
(let ((*input* "let x = compile-expr print x"))
  (defun compile-compiler (word)
    "compiles a *-COMPILER-* word"
   compile-expr))
#+nil
(let ((*input* "let y = make-table print y"))
  (compile-expr))
#+nil
(let ((*input* "print x"))
  (call-in (list (gethash  )
  (compile-expr))
#+nil
(let ((*input* "word hello let x = compile-expr print quote word print x end print quote world"))
  ("hello" *-compiler-*))))
#+nil
(let ((*input* "hello true"))
  (compile-expr))
#+nil
(let ((*input* "let x = 'hello world' print x"))
  (compile-expr))
(defun repl ()
  (loop
    (let ((*input* (read-line)))
      (call-in (compile-expr)))))
