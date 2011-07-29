; Mini-CProlog
; resol.lsp
;


(defun forward ()			; forward process
  (do () ((null Duboulot) (format t "no More ~%"))
    (cond
      ((null CP) (answer))		; empty resolvent
      ((load_PC)			; selects the first goal
       (cond 
	 ((user? PC)			; user defined
	  (let ((d (def_of PC)))	; associated set of clauses
	    (if d (pr d) (backtrack))))
	 ((builtin? PC)			; builtin predicate
	  (if (eq (apply (car PC) (cdr PC)) 'fail)
	      (backtrack)		; evaluation fails
	      (cont_det)))		; else new continuation
	 ((backtrack)))))))		; undefined predicate

(defun load_PC ()			;first goal and its env
  (setq PC (car CP) PCE (E CL) PCG (G CL)))

(defun pr (paq)				; if more than one clause 
  (if (cdr paq)				; allocates a rerun part
      (progn (push_choix) (pr_choice paq))
    (pr_det (car paq))))

(defun pr_det (c)			; only one clause candidate
  (if (eq (unify (largs PC) PCE PCG 
		 (largs (head c)) (push_E (nloc c)) (push_G (nglob c))) 
          'fail)			; tries to unify
      (backtrack)			; failure
    (progn				; success
      (if (tail c)			; for a rule
	  (progn (push_cont) (setq CP (tail c) CL L) (maj_L (nloc c)))
	(cont_det))			; for a fact
      (maj_G (nglob c)))))

(defun cont_det ()			; looks for next continua.
  (if (cdr CP)				; and siblings ?
      (setq CP (cdr CP))		; done
    (progn				; no intermediate choice pts
      (if (< BL CL) (setq L CL))	; updates the local stack
      (do ()				; searching next cont.
	  ( (or (cdr (CP CL)) (zerop (CL CL)))
	    (setq CP (cdr (CP CL)) CL (CL CL)))
	  (setq CL (CL CL))
	  (if (< BL CL) (setq L CL))))))

(defun pr_choice (paq)			; more than one candidate
  (let* ((resu (shallow_backtrack paq)) ; determines first success
	 (c (car resu)) 
	 (r (cdr resu)))
    (cond ((null r)			; only one candidate remains
	   (pop_choix)			; pops the rerun part
	   (pr_det c))			; proof is now deterministic
	  ( (push_bpr r)		; else updates the field BP
	    (push_cont)			; saves current continuation         
	    (if (tail c)		; looking for the new one
		(setq CP (tail c) CL L)
	      (if (cdr CP)
		  (setq CP (cdr CP))    
		(do ()
		    ((or (cdr (CP CL)) (zerop (CL CL)))
		     (setq CP (cdr (CP CL)) CL (CL CL)))
		    (setq CL (CL CL)))))
	    (maj_L (nloc c))		; terminates local block
	    (maj_G (nglob c))))))	; terminates global block

(defun shallow_backtrack (paq)		; looks for a first success
  (if (and (cdr paq)			; more than one candidate
	   (eq (unify (largs PC)	; and unification fails
		      PCE
		      PCG
		      (largs (head (car paq)))
		      (push_E (nloc (car paq)))
		      (push_G (nglob (car paq))))
	       'fail))
      (progn (poptrail (TR BL))		; restores the environments
	     (shallow_backtrack (cdr paq))) ; again with next clause
    paq))
              
(defun backtrack ()
  (if (zerop BL)			; no more choice points
      (setq Duboulot nil)		; neds the proof
    (progn				; updates backtracking 
      (setq L BL G BG			; specific registers
	    PC (car (CP L))		; restore the goal and 
	    PCE (E (CL L)) PCG (G (CL L)) ; its environments
	    CP (CP L) CL (CL L))
      (poptrail (TR BL))		; restores the environments
      (pr_choice (BP L)))))		; restarts the proof
   
(defun myloop (c)			; top level loop
  (setq G BottomG L BottomL TR BottomTR ; init registers
	CP nil CL 0  BL 0 BG BottomG Duboulot t)
  (push_cont)				; saves empty continuation
  (push_E (nloc c))			; allocates env for query
  (push_G (nglob c)) 
  (setq CP (cdr c) CL L)		; current continuation
  (maj_L (nloc c))			; terminates block
  (maj_G (nglob c)) (read-char)
  (catch 'debord (forward))		; to catch stacks overflow
  (myloop (read_prompt)))




