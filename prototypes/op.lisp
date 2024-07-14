;;; Some quick lisp code to implement arbitrary operator parsing for a general operator
;;; table

(defparameter *op-table*
  '((+ 600)
    (- 600)
    (* 500)
    (/ 500)))


(defparameter *tokens* '())

(defun peek-next ()
  "Gives the next token in *TOKENS* but does not consume it"
  (car *tokens*))
(defun consume ()
  "Consumes one character from *TOKENS* and returns it. Returns nil if no more tokens"
  (prog1
    (car *tokens*)
    (setf *tokens* (cdr *tokens*))))

(defun token-precedence (token)
  (or (cadr (assoc token *op-table*)) 0))

;;;; Simple rule for parsing:
;;;;
;;;; The precedence of all subtrees must be less than or equal to the precedence of the
;;;; current operator. If not, try again with another.
(defun parse (&key (stack nil) (precedence 1400))
  "Guarantees the precedence of the operator of the return value is less than PRECEDENCE"
  (format t "~s~%-----------------------~%" *tokens*)
  (let* ((curr (peek-next))
         (prec (token-precedence curr)))
     (cond
       ((null curr) stack)
       ((= 0 prec) (consume) (parse :stack curr :precedence precedence))
       ((< prec precedence)
         (consume)
         (parse :stack (list curr stack (parse :stack '() :precedence prec)) :precedence precedence))
       (t stack))))
#+nil
(let ((*tokens* '(10)))
  (parse))
#+nil
(let ((*tokens* '(10 * 20 * 40 + 20 * 30 / 40)))
  (parse))
