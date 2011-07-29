; mini-Wam
; utili.lsp
;

(defmacro nvar (c) `(car ,c))		; number of vars
(defmacro head (c) `(cadr ,c))		; head of a clause
(defmacro tail (c) `(cddr ,c))		; body of a rule
(defmacro pred (g) `(car ,g))		; predicate symbol

(defmacro user? (g)			; user defined predicate
  `(get (pred ,g) 'def))
(defmacro builtin? (g)			; builtin predicate
  `(get (pred ,g) 'evaluable))
(defmacro def_of (g)			; gets the subset of clauses
  `(get (pred ,g) 
	(if (largs ,g) (nature (ultimate (car (largs ,g)) PCE)) 'def)))

(defun nature (te)			; determines the associated
  (cond					; subset according to
   ((var? te) 'def)			; clause indexing
   ((null te) 'empty)
   ((atom te) 'atom)
   ((list? te) 'list)
   (t 'fonct)))

(defun add_cl (pred c ind)		; adds a clause to the end
  (setf (get pred ind) (append (get pred ind) (list c))))

(set-macro-character			; to automatically read,
 #\$					; code and index
 #'(lambda (stream char)
     (let* ( (*standard-input* stream) (c (read_code_cl)))
       (add_cl (pred (head c)) c 'def)	; always in the var subset
       (if (largs (head c)) 
	   (let ((b (nature (car (largs (head c))))))
	     (if (eq b 'def)		; first arg is a var
		 (mapc			; add c to all subsets
		  #' (lambda (x) (add_cl (pred (head c)) c x))
		  '(atom empty list fonct))
	       (add_cl (pred (head c)) c b))))) ; add to only one
     (values)))
  
(defun answer ()			; the answer to a query
  (printvar)				; resulting bindings
  (if (zerop BL)			; no more choice points
      (setq Duboulot nil)
    (if (and (princ "More : ")		; asks the user
	     (string= (read-line) ";")) ; if necessary
	(backtrack)			; forces backtracking
      (setq Duboulot nil))))
	
(defun printvar ()			; prints the values of vars
  (if (null *lvar)			; ground query
      (format t "Yes ~%")
    (let ((n -1))
      (mapc				; proceeds each variable
       #' (lambda (x)
	    (format t "~A = " x)
	    (write1 (ult (+ (incf n) (E BottomL)))) (terpri))
       *lvar))))

