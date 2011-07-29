; mini-Wam
; unify.lsp
;
(defun ult (n)				; dereferences a var
  (let ((te (svref Mem n))) 
    (if (and (var? te) (/= (cdr te) n)) (ult (cdr te)) te)))
  
(defun ultimate (x e) (if (var? x) (ult (adr x e)) x)) 
(defun val (x) (if (var? x) (ult (cdr x)) x))

(defmacro bind (x te)			; binds x to instance te
  `(progn				; if post-bound
     (if (or (and (> ,x BottomL) (< ,x BL)) (<= ,x BG)) 
	 (pushtrail ,x))		; trail it
     (vset Mem ,x ,te)))
(defun bindte (xadr y)			; atom or instance
  (if (or (atom y) (copy? y))
      (bind xadr y)			; direct
    (bind xadr (recopy y (E L)))))	; else constructing
(defun genvar (x) (bind x (push_G (cons 'V G))))
(defmacro bindv (x y)			; L2 binding
  `(if (< (cdr ,x) (cdr ,y)) (bind (cdr ,y) ,x) (bind (cdr ,x) ,y))) 

(defun unify_with (largs e)           	; unify head of clause
  (catch 'impossible			; with registers Ai
    (do ((i (1+ A) (1+ i)))
	((null largs))
	(unif (svref Mem i) (ultimate (pop largs) e)))))
                 
(defun unif (x y)			; unify two terms
  (cond
   ((eql x y) t)
   ((var? y) (if (var? x)
		 (if (= (cdr x) (cdr y)) t (bindv y x))
	       (bindte (cdr y) x)))
   ((var? x) (bindte (cdr x) y))
   ((or (atom x) (atom y)) (throw 'impossible 'fail))
   ((let ((b (copy? y)) (dx (pop x)) (dy (pop y)))
      (if (and (eq (functor dx) (functor dy))
	       (= (arity dx) (arity dy)))
	  (do () ((null x))
	      (unif (val (pop x)) 
		    (if b (val (pop y)) (ultimate (pop y) (E L)))))
	(throw 'impossible 'fail))))))
