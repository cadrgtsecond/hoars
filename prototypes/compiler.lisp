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
    (setf *input* (str:s-rest *input*))
    "string-quote"))
(defun read-token ()
  (or (read-string-quote) (read-word)))

(defun peek-token ()
  "Peeks a token without consuming it"
  (let ((*input* *input*))
    (read-token)))

(defparameter *-values-* (make-hash-table :test #'equal)
  "The hash table that maps variable names to values")

(define-symbol-macro *-compiler-* (gethash "-compiler-" *-values-*))
(setf *-compiler-* (make-hash-table :test #'equal))
;;; The *-compiler-* maps names to "instructions", not functions

(defparameter *stack* '()
  "The stack which stores values")
(defun call-in (in)
  "Calls the given instruction, returning the top of the stack if it is NIL"
  (if in
      (funcall (car in) in)
      (car *stack*)))


(defmacro with-push (values &body body)
  "Pushes VALUES onto the stack in the reverse order specified"
  `(let ((*stack* (list* ,@values *stack*)))
     ,@body))
(defmacro with-pop (vars &body body)
  "Pops VARS from the stack in the order specified"
  `(destructuring-bind (,@vars &rest *stack*) *stack*
     ,@body))

(defun compile-expr ()
  "Compiles an expression from the input onto the stack"
  (let* ((token (read-token))
         (in (gethash token *-compiler-*)))
    (when token
      (if in
         (call-in in)
         (error "Unknown word")))))
(setf (symbol-function 'wcompile-expr) (native compile-expr 0))

;;; We could use a regular instruction for this, but a macro make this faster for common code
(defmacro native (fn &optional arity)
  "Generates instruction to call FN as a native lisp function passing ARITY number of arguments from the stack"
  (let ((names (when arity
                  (loop for i from 1 to arity
                        collect (gensym)))))
    `(lambda (self)
       (destructuring-bind (,@names &rest *stack*) *stack*
         ;;; We must reverse the order so that order in the language is equal to order in CL
         (with-push ((,fn ,@(reverse names)))
           (call-in (cdr self)))))))

(defmacro defword (name dict arglist &body code)
  "Defines a word named NAME in DICT that takes SELF in ARGLIST and returns CODE"
  `(setf (gethash ,name ,dict)
     (list (lambda ,arglist ,@code))))

(defmacro defcompiler (name value)
  `(setf (gethash ,name *-compiler-*) ,value))

(defcompiler "compile-expr" '(push-val (wcompile-expr)))
(defcompiler "undefined" '(push-val (push-val nil)))
;;; String
(defword "string-quote" *-compiler-* (self)
  (destructuring-bind (val &optional rest)
      (str:split "['\"]" *input* :omit-nulls t :limit 2 :regex t)
    (setf *input* (str:trim-left rest))
    (list 'push-val val)))

(defun push-val (self)
  (with-push ((cadr self))
    (call-in (cddr self))))

;; NOTE: read-token is actually executed when quote is compiled
(defcompiler "quote" `(push-val (push-val) ,(native read-token) ,(native list 1) join))
(defun join (self)
  "Combines the two values on top of the stack"
  (with-pop (a2 a1)
    (with-push ((append a1 a2))
      (call-in (cdr self)))))
(defcompiler "join" `(join))


(defun print-value (arg)
  (format t "=> ~a~%" arg)
  arg)
(defcompiler "print" `(wcompile-expr push-val (,(native print-value 1)) join))

(defun bind-var (self)
  (with-pop (val)
    (setf (gethash (cadr self) *-values-*) val)
    (call-in (cddr self))))
(defun get-var (self)
  (with-push ((gethash (cadr self) *-values-*))
    (call-in (cddr self))))
(defword "let" *-compiler-* (self)
  (let ((name (read-word))
        (sep (read-token)))
    (when (string= sep "=")
      (defcompiler name `(push-val (get-var ,name)))
      `(,@(compile-expr) bind-var ,name ,@(compile-expr)))))

;;;; Now that we have a way to define variables, let us define `-compiler-` properly as a variable
(let ((*input* "let -compiler- = undefined print -compiler-"))
  ;; Since we do not execute the expression, the value of -compiler- is never changed
  (compile-expr))

(defun call (self)
  (with-push ((call-in (cadr self)))
    (call-in (cddr self))))
(defword "word" *-compiler-* (self)
  (let ((name (read-word))
        (code (loop until (string= (peek-token) "end")
                    appending (compile-expr)
                    finally (read-token))))
    (defcompiler name `(call ,code))
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
        `(,@condition cnjump ,rest ,@then-body ,@rest))
      (let ((else-body (loop until (string= (peek-token) "end")
                             appending (compile-expr)
                             finally (read-token)))
            (rest (compile-expr)))
        `(,@condition cnjump (,@else-body ,@rest) ,@then-body ,@rest)))))

(defun wget (object index)
  ;; TODO: Implement more types of objects
  (gethash index object))

(defcompiler "get" `(wcompile-expr wcompile-expr push-val (,(native wget 2)) join join))

(defun wmake-table ()
  (make-hash-table :test #'equal))
(defcompiler "make-table" `(push-val (,(native wmake-table))))
(defword "make-table" *-compiler-* (self)
  `((wmake-table)))

#+nil
(let ((*input* "print get -compiler- '-compiler-'"))
  (compile-expr))

#+nil
(let ((*input* "let y = quote hello"))
  (compile-expr))
#+nil
(let ((*input* "print x"))
  (call-in (list (gethash  )
  (compile-expr))
#+nil
(let ((*input* "word hello let x = compile-expr print quote x print x end print quote world"))
  (compile-expr))
#+nil
(let ((*input* "hello true"))
  (compile-expr))
#+nil
(let ((*input* "if true then print 'hello'"))
  (compile-expr))
#+nil
(let ((*input* "let x = 'hello world' print x"))
  (compile-expr))
(defun repl ()
  (loop
    (let ((*input* (read-line)))
      (call-in (compile-expr)))))
