;;;; Prototype compiler

(ql:quickload :str)

(defparameter *input* "")

(defun read-word ()
  "Reads a token from the input, moving it forward. Returns NIL if no more output"
  (destructuring-bind (&optional res rest) (str:split "\\s+" *input* :limit 2 :omit-nulls t :regex t)
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

(defun compile-expr ()
  "Compiles an expression from the input"
  (let ((token (read-token)))
    (and token (call-in (list (or (gethash token *-compiler-*) (error "Unknown word")))))))
(defun wcompile-expr (self)
  (let ((*stack* (list* (compile-expr) *stack*)))
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
  (push (gethash (cadr self) *-values-*) *stack*)
  (call-in (cddr self)))

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

#+nil
(let ((*input* "let x = compile-expr print x"))
  (compile-expr))
#+nil
(let ((*input* "word hello let x = compile-expr print quote word print x end print quote world"))
  (compile-expr))
#+nil
(let ((*input* "print x"))
  (call-in (list (gethash "hello" *-compiler-*))))
#+nil
(let ((*input* "hello true"))
  (compile-expr))
#+nil
(let ((*input* "if true print quote hello else print quote world end print quote final"))
  (compile-expr))
#+nil
(let ((*input* "if false print quote hello else print quote world end print quote final"))
  (compile-expr))
#+nil
(let ((*input* "if true print quote hello end print quote final"))
  (compile-expr))
#+nil
(let ((*input* "if false print quote hello end print quote final"))
  (compile-expr))
