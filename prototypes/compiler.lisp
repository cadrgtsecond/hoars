;;;; Prototype compiler

(ql:quickload :str)

(defparameter *input* "")
(defun read-token ()
  "Reads a token from the input, moving it forward. Returns NIL if no more output"
  (destructuring-bind (&optional res rest) (str:split " " *input* :limit 2 :omit-nulls t)
    (setf *input* rest)
    res))
(defun peek-token ()
  "Peeks a token without consuming it"
  (let ((*input* *input*))
    (read-token)))

(defparameter *-compiler-* (make-hash-table :test #'equal))

(setf (gethash "quote" *-compiler-*)
(lambda ()
  (list '(push-val) (read-token))))

(defun compile-expr ()
  "Compiles an expression from the input"
  (funcall (gethash (read-token) *-compiler-*)))

#+nil
(setf *input* "quote hello to my friends")
#+nil
(compile-expr)
