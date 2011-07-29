; mini-Wam
; resol.lsp
;

(defun forward ()
  (do () ((null Duboulot) (format t "no More ~%"))
      (cond ((null CP) (answer))	; empty resolvent
	    ( (load_PC)			; selects first goal
	      (cond          
	       ((user? PC)		; user defined
		(let ((d (def_of PC)))	; associated set of clauses
		  (if d (pr2 d) (backtrack))))
	       ((builtin? PC)		; builtin
		(if (eq (apply (car PC) (cdr PC)) 'fail)
		    (backtrack)		; evaluation fails   
		  (cont_eval)))         ; else new continuation
	       ((backtrack)))))))	; undefined predicate
         
(defun load_PC ()
  (setq PC (pop CP) PCE (E CL) Cut_pt BL))

(defun load_A (largs el)		; loads Ai registers with
  (dotimes (i (length largs) (vset Mem A i)) ; goal's args
	   (vset Mem 
		 (+ A i 1) 
		 (let ((te (ultimate (pop largs) el)))
		   (cond
		    ((atom te) te)
		    ((var? te) (if (unsafe? te) (genvar (cdr te)) te))
                    ((copy? te) te)
		    ((recopy te el))))))) 

(defun unsafe? (x)			; last call and current env
  (and (not CP) (>= (cdr x) CL)))

(defun pr2 (paq)			; proves PC
  (load_A (largs PC) PCE)		; loads its arguments
  (if CP 
      (pr paq)
    (progn				; if last call and no interm.
      (if (<= BL CL) (setq L CL))	; choice points then LCO
      (setq CP (CP CL) CL (CL CL))	; next continuation
      (pr paq))))
        
(defun cont_eval ()
  (unless CP
	  (if (<= BL CL) (setq L CL)) (setq CP (CP CL) CL (CL CL))))

(defun pr (paq) 
  (if (cdr paq)				; alloc choice point
      (progn (push_choix) (pr_choice paq))
    (pr_det (car paq))))		; else deterministic

(defun pr_det (c)
  (if (eq (unify_with (largs (head c)) (push_E (nvar c))) 'fail)
      (backtrack)                
    (when (tail c)			; c is a rule
	  (push_cont)			; saves current cont.
	  (setq CP (tail c) CL L)	; new one
	  (maj_L (nvar c)))))		; terminates local block

(defun pr_choice (paq)
  (let* ((resu (shallow_backtrack paq)) (c (car resu)) (r (cdr resu)))
    (cond ((null r)			; only one candidate remains
	   (pop_choix)			; pops the rerun part
	   (pr_det c))			; proof is now deterministic
	  ( (push_bpr r)                   
	    (when (tail c)		; c is a rule
		  (push_cont)		; saves current cont.
		  (setq CP (tail c) CL L) ; new one
		  (maj_L (nvar c))))))) ; terminates local block

(defun shallow_backtrack (paq)		; looks for a first success
  (if (and (cdr paq)			; more than one candidate 
	   (eq (unify_with		; and unification fails
		(largs (head (car paq)))
		(push_E (nvar (car paq)))) 'fail))
      (progn
	(poptrail (TR BL))		; restores env
	(setq G BG)
	(shallow_backtrack (cdr paq)))
    paq))
              
(defun backtrack ()                        
  (if (zerop BL)			; no more choice points
      (setq Duboulot nil)              
    (progn				; restores registers
      (setq L BL G BG Cut_pt (BL BL) CP (BCP L) CL (BCL L))
      (load_A2)           
      (poptrail (TR BL))		; restores env.        
      (pr_choice (BP L)))))		; relaunch the proof
   
(defun load_A2 ()			; restores Ai registers
  (let ((deb (- L (size_C L))))
    (dotimes (i (A L) (vset Mem A i))
	     (vset Mem (+ A i 1) (svref Mem (+ deb i))))))

(defun myloop (c)
  (setq G BottomG L BottomL TR BottomTR ; inits registers
        CP nil CL 0 BL 0 BG BottomG Duboulot t Cut_pt 0)
  (push_cont)				; initial continuation
  (push_E (nvar c))			; env. for the query
  (setq CP (cdr c) CL L)		; current continuation
  (maj_L (nvar c))  (read-char)		; ends block
  (catch 'debord (forward))
  (myloop (read_prompt)))


