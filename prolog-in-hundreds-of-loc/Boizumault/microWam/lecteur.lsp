; mini-Wam
; parser.lsp
;

(defvar *lvar nil)			; list of vars
(set-macro-character #\% (get-macro-character #\;))

(defun rch ()				; skips Newline
  (do ((ch (read-char) (read-char)))
      ((char/= ch #\Newline) ch)))
(defun rchnsep ()			; skips Newline and Space
  (do ((ch (rch) (rch)))
      ((char/= ch #\space) ch)))

(defun special (ch) (char= ch #\_))
(defun alphanum (ch) (or (alphanumericp ch) (special ch)))
(defun valdigit (ch) (digit-char-p ch)) 
 
(defun read_number (ch)			; next integer
  (do ((v (valdigit ch) (+ (* v 10) (valdigit (read-char)))))
      ((not (digit-char-p (peek-char))) v)))

(defun implode (lch) (intern (map 'string #'identity lch)))

(defun read_atom (ch)			; next normal atom
  (do ((lch (list ch) (push (read-char) lch)))
      ((not (alphanum (peek-char))) (implode (reverse lch)))))

(defun read_at (ch)			; next special atom
  (do ((lch (list ch) (push (read-char) lch)))
      ((char= (peek-char) #\') (read-char) (implode (reverse lch)))))

(defun read_string (ch)			; Prolog list of Ascii codes
  (do ((lch (list (char-int ch)) (push (char-int (read-char)) lch)))
      ((char= (peek-char) #\") (read-char) (do_l (reverse lch)))))

(defun read_var (ch)			; next variable
  (let ((v (read_atom ch)))
    (cons 'V
	  (position v (if (member v *lvar)
			  *lvar
			(setq *lvar (append *lvar (list v))))))))

(defun read_simple (ch)			; next simple term
  (cond
   ((or (special ch) (upper-case-p ch)) (read_var ch))
   ((digit-char-p ch) (read_number ch))
   ((char= ch #\") (read_string (read-char)))
   ((char= ch #\') (read_at (read-char)))
   (t (read_atom ch))))

(defun read_fct (ch)			; next functional term
  (let ((fct (read_simple ch)) (c (rchnsep)))
    (if (char= c #\()			; reads its argument
	(let ((la (read_args (rchnsep)))) 
	  (cons (list fct (length la)) la)) ; adds its descriptor
      (progn (unread-char c) fct))))
                
(defun read_args (ch)			; args of a functional term
  (let ((arg (read_term ch)))
    (if (char= (rchnsep) #\,)
	(cons arg (read_args (rchnsep)))
      (list arg))))
   
(defun read_list (ch)			; next list
  (if (char= ch #\])
      ()				; empty list
    (let ((te (read_term ch)))		; gets the head
      (case (rchnsep)
	    (#\, (list '(\. 2) te (read_list (rchnsep))))
	    (#\| (prog1			; dotted pair
		     (list '(\. 2) te (read_term (rchnsep))) 
		   (rchnsep))) 
	    (#\] (list '(\. 2) te nil))))))

(defun read_term (ch)			; next term
  (if (char= ch #\[) (read_list (rchnsep)) (read_fct ch))) 

(defun read_tail (ch)			; reads the body of a clause
  (let ((tete (read_pred ch)))
    (if (char= (rchnsep) #\.)
	(list tete)			; first goal
      (cons tete (read_tail (rchnsep))))))

(defun read_clause (ch)			; reads a clause
  (let ((tete (read_pred ch)))		; gets the head
    (if (char= (rchnsep) #\.)
	(list tete)			; a fact
      (progn (read-char)		; else a rule
	     (cons tete (read_tail (rchnsep))))))) 

(defun read_code_cl ()			; reads and codes a clause
  (let ((*lvar ()))
    (let ((x (read_clause (rchnsep)))) 
      (cons (length *lvar) (c x)))))
              
(defun read_code_tail ()		; reads a query
  (setq *lvar ())
  (let ((x (read_tail (rchnsep))))
    (cons (length *lvar) (append (c x) (list '(|true|))))))

(defun c (l)
  (if l (cons (if (eq (caar l) '!) 
		  (list '! (length *lvar)) ; the cut and env size
		(car l))
	      (c (cdr l)))))

(defun read_pred (ch)			; reads a predicate
  (let ((nom (read_atom ch)) (c (rchnsep)))
    (if (char= c #\()
	(cons nom (read_args (rchnsep))) 
      (progn (unread-char c) (list nom)))))
