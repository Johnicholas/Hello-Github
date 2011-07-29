; Version 2
; boot.lsp
;
  
(defun mlg2 ()
  (banner)
  (format t "~A~%" (load "../microUt/mlg.Start"))
					;(setq *gc-silence* t)  
  (myloop (read_prompt)))

(defun read_prompt () 
  (terpri)
  (format t "| ?- ")
					;(gc)
  (read_code_tail))

(defun banner ()
  (dotimes (i 2) (terpri))
  (format t "Micro_Log2 pour vous servir~%")
  (dotimes (i 2) (terpri)))

(defun l ()
  (format t "Back to MicroLog2 top-level~%")
  (myloop (read_prompt)))
; Version 2
; lecteur.lsp
;

(defvar *lvar nil)
(set-macro-character #\% (get-macro-character #\;))

(defun rch ()
  (do ((ch (read-char) (read-char)))
      ((char/= ch #\Newline) ch)))
(defun rchnsep ()	
  (do ((ch (rch) (rch)))
      ((char/= ch #\space) ch)))

(defun special (ch) (char= ch #\_))
(defun alphanum (ch) (or (alphanumericp ch) (special ch)))
(defun valdigit (ch) (digit-char-p ch)) 
 
(defun read_number (ch) 
  (do ((v (valdigit ch) (+ (* v 10) (valdigit (read-char)))))
      ((not (digit-char-p (peek-char))) v)))

(defun implode (lch) (intern (map 'string #'identity lch)))

(defun read_atom (ch) 
  (do ((lch (list ch) (push (read-char) lch)))
      ((not (alphanum (peek-char))) (implode (reverse lch)))))
(defun read_at (ch)
  (do ((lch (list ch) (push (read-char) lch)))
      ((char= (peek-char) #\') (read-char) (implode (reverse lch)))))

(defun read_string (ch)
  (do ((lch (list (char-int ch)) (push (char-int (read-char)) lch)))
      ((char= (peek-char) #\") (read-char) (do_l (reverse lch)))))

(defun read_var (ch) 
  (let ((v (read_atom ch)))
    (cons 'V
	  (position v (if (member v *lvar)
			  *lvar
			(setq *lvar (append *lvar (list v))))))))

(defun read_simple (ch)
  (cond
   ((or (special ch) (upper-case-p ch)) (read_var ch))
   ((digit-char-p ch) (read_number ch))
   ((char= ch #\") (read_string (read-char)))
   ((char= ch #\') (read_at (read-char)))
   (t (read_atom ch))))

(defun read_fct (ch)
  (let ((fct (read_simple ch)) (c (rchnsep)))
    (if (char= c #\()
	(let ((la (read_args (rchnsep)))) (cons (list fct (length la)) la))
      (progn (unread-char c) fct))))
                
(defun read_args (ch) 
  (let ((arg (read_term ch)))
    (if (char= (rchnsep) #\,)
	(cons arg (read_args (rchnsep)))
      (list arg))))
   
(defun read_list (ch)
  (if (char= ch #\])
      ()
    (let ((te (read_term ch)))
      (case (rchnsep)
	    (#\, (list '(\. 2) te (read_list (rchnsep))))
	    (#\| (prog1 (list '(\. 2) te (read_term (rchnsep))) (rchnsep))) 
	    (#\] (list '(\. 2) te nil))))))

(defun read_term (ch) (if (char= ch #\[) (read_list (rchnsep)) (read_fct ch))) 

(defun read_tail (ch)	
  (let ((tete (read_pred ch)))
    (if (char= (rchnsep) #\.)
	(list tete)
      (cons tete (read_tail (rchnsep))))))

(defun read_clause (ch) 
  (let ((tete (read_pred ch)))
    (if (char= (rchnsep) #\.)
	(list tete)
      (progn (read-char) (cons tete (read_tail (rchnsep)))))))

(defun read_code_cl () 
  (let ((*lvar ()))
    (let ((x (read_clause (rchnsep)))) (cons (length *lvar) (c x)))))
              
(defun read_code_tail ()
  (setq *lvar ())
  (let ((x (read_tail (rchnsep))))
    (cons (length *lvar) (append (c x) (list '(|true|))))))

(defun c (l)
  (if l (cons (if (eq (caar l) '!) (list '! (length *lvar)) (car l))
	      (c (cdr l)))))

(defun read_pred (ch) 
  (let ((nom (read_atom ch)) (c (rchnsep)))
    (if (char= c #\()
	(cons nom (read_args (rchnsep))) 
      (progn (unread-char c) (list nom)))))
; Version 2
; blocs.lsp
;

; I. Registres
;

(defconstant BottomG 0)
(defconstant BottomL 2000)
(defconstant BottomTR 8000)
(defconstant A 10000)

(defvar Mem (make-array 10050 :initial-element 0))
(defvar TR)
(defvar L)
(defvar G)
(defvar CP)
(defvar CL)
(defvar Cut_pt)
(defvar BL)
(defvar BG)
(defvar PC)
(defvar PCE)
(defvar Duboulot)

(defmacro vset (v i x) `(setf (svref ,v ,i) ,x))
(defmacro tcons (des largs) `(cons (append ,des (list '#\*)) ,largs))
(defmacro des (te) `(car ,te))
(defmacro arity (des) `(cadr ,des))
(defmacro functor (des) `(car ,des))
(defmacro largs (x) `(cdr ,x))
(defmacro var? (x) `(and (consp ,x) (numberp (cdr ,x))))
(defmacro list? (x) `(eq (functor (des ,x)) '\.))

; II. Local Stack
;

; WAM notion of environment [CL CP Cut E]
;
(defmacro CL (b) `(svref Mem ,b))
(defmacro CP (b) `(svref Mem (1+ ,b)))
(defmacro Cut (b) `(svref Mem (+ ,b 2)))
(defmacro E (b) `(+ ,b 3))            

(defmacro push_cont ()           
  `(progn (vset Mem L CL) (vset Mem (1+ L) CP)))
  
(defmacro push_E (n)		
  `(let ((top (+ L 3 ,n)))	
     (if (>= top BottomTR)
	 (throw 'debord (print "Local Stack Overflow")))
     (vset Mem (+ L 2) Cut_pt)	
     (dotimes (i ,n top) (vset Mem (decf top) (cons 'V top)))))
 
(defmacro maj_L (nl) `(incf L (+ 3 ,nl)))


;choice-point : [a1 .. an A BCP BCL BG BL BP TR]
;
(defmacro TR (b) `(svref Mem (1- ,b)))
(defmacro BP (b) `(svref Mem (- ,b 2)))
(defmacro BL (b) `(svref Mem (- ,b 3)))
(defmacro BG (b) `(svref Mem (- ,b 4)))
(defmacro BCL (b) `(svref Mem (- ,b 5)))
(defmacro BCP (b) `(svref Mem (- ,b 6)))
(defmacro A (b) `(svref Mem (- ,b 7)))

(defun save_args ()
  (dotimes (i (svref Mem A) (vset Mem (incf L i) i))
	   (vset Mem (+ L i) (svref Mem (+ A i 1)))))

(defun push_choix ()
  (save_args)
  (vset Mem (incf L) CP)
  (vset Mem (incf L) CL)
  (vset Mem (incf L) G)     
  (vset Mem (incf L) BL)
  (vset Mem (incf L 2) TR) 
  (setq BL (incf L) BG G))
        
(defun push_bpr (reste) (vset Mem (- BL 2) reste))
(defmacro size_C (b) `(+ 7 (A ,b)))    

(defun pop_choix () 
  (setq L (- BL (size_C BL)) BL (BL BL) BG (if (zerop BL) BottomG (BG BL))))

; III. Copy Stack (Heap)
;

(defmacro push_G (x)
  `(if (>= (incf G) BottomL) 
       (throw 'debord (print "Heap Overflow"))
     (vset Mem G ,x)))

(defmacro adr (v e) `(+ (cdr ,v) ,e))

(defun copy (x e)
  (cond
   ((var? x) (let ((te (ult (adr x e))))
	       (if (var? te) (genvar (cdr te)) te)))
   ((atom x) x)
   ((tcons (des x) (cop (largs x) e)))))

(defun cop (l e) 
  (if l (cons (copy (car l) e) (cop (cdr l) e))))

(defmacro recopy (x e) `(push_G (copy ,x ,e)))
(defmacro copy? (te) `(cddr (des ,te)))

;IV. Trail
;

(defmacro pushtrail (x)
  `(cond ((>= TR A) (throw 'debord (print "Trail Overflow")))
	 ((vset Mem TR ,x) (incf TR))))  

(defmacro poptrail (top)	       
  `(do () ((= TR ,top))
       (let ((v (svref Mem (decf TR)) )) (vset Mem v (cons 'V v)))))
; Version 2
; utili.lsp
;

(defmacro nvar (c) `(car ,c))
(defmacro head (c) `(cadr ,c))
(defmacro tail (c) `(cddr ,c))
(defmacro pred (g) `(car ,g))

(defmacro user? (g) `(get (pred ,g) 'def))
(defmacro builtin? (g) `(get (pred ,g) 'evaluable))
(defmacro def_of (g) 
  `(get (pred ,g) 
	(if (largs ,g) (nature (ultimate (car (largs ,g)) PCE)) 'def)))

(defun nature (te)
  (cond 
   ((var? te) 'def)
   ((null te) 'empty)
   ((atom te) 'atom)
   ((list? te) 'list)
   (t 'fonct)))

(defun add_cl (pred c ind)
  (setf (get pred ind) (append (get pred ind) (list c))))

(set-macro-character
 #\$
 #'(lambda (stream char)
     (let* ( (*standard-input* stream) (c (read_code_cl)))
       (add_cl (pred (head c)) c 'def)
       (if (largs (head c)) 
	   (let ((b (nature (car (largs (head c))))))
	     (if (eq b 'def)
		 (mapc 
		  #' (lambda (x) (add_cl (pred (head c)) c x))
		  '(atom empty list fonct))
	       (add_cl (pred (head c)) c b)))))
     (values)))
  
(defun answer ()
  (printvar)
  (if (zerop BL)
      (setq Duboulot nil)
    (if (and (princ "More : ") (member (read) '(o y)))
	(backtrack)
      (setq Duboulot nil))))
	
(defun printvar ()
  (if (null *lvar)
      (format t "Yes ~%")
    (let ((n -1))
      (mapc 
       #' (lambda (x)
	    (format t "~A = " x)
	    (write1 (ult (+ (incf n) (E BottomL)))) (terpri))
       *lvar))))

; Version 2
; unify.lsp
;

(defun ult (n) 
  (let ((te (svref Mem n))) 
    (if (and (var? te) (/= (cdr te) n)) (ult (cdr te)) te)))
  
(defun ultimate (x e) (if (var? x) (ult (adr x e)) x)) 
(defun val (x) (if (var? x) (ult (cdr x)) x))

(defmacro bind (x te)
  `(progn 
     (if (or (and (> ,x BottomL) (< ,x BL)) (<= ,x BG)) (pushtrail ,x))
     (vset Mem ,x ,te)))

(defun bindte (xadr y)
  (if (or (atom y) (copy? y))
      (bind xadr y)
    (bind xadr (recopy y (E L)))))

(defun genvar (x) (bind x (push_G (cons 'V G))))

(defmacro bindv (x y)
  `(if (< (cdr ,x) (cdr ,y)) (bind (cdr ,y) ,x) (bind (cdr ,x) ,y))) 

(defun unify_with (largs e)           	
  (catch 'impossible 
    (do ((i (1+ A) (1+ i)))
	((null largs))
	(unif (svref Mem i) (ultimate (pop largs) e)))))
                 
(defun unif (x y)
  (cond
   ((eql x y) t)
   ((var? y) (if (var? x)
		 (if (= (cdr x) (cdr y)) t (bindv y x))
	       (bindte (cdr y) x)))
   ((var? x) (bindte (cdr x) y))
   ((or (atom x) (atom y)) (throw 'impossible 'fail))
   ((let ((b (copy? y)) (dx (pop x)) (dy (pop y)))
      (if (and (eq (functor dx) (functor dy)) (= (arity dx) (arity dy)))
	  (do () ((null x))
	      (unif (val (pop x)) 
			 (if b (val (pop y)) (ultimate (pop y) (E L)))))
	(throw 'impossible 'fail))))))
; Version 2
; resol.lsp
;

(defun forward ()
  (do () ((null Duboulot) (format t "no More ~%"))
      (cond ((null CP) (answer))
	    ( (load_PC)  
	      (cond          
	       ((user? PC) 
		(let ((d (def_of PC))) (if d (pr2 d) (backtrack))))
	       ((builtin? PC)   
		(if (eq (apply (car PC) (cdr PC)) 'fail)
		    (backtrack)             
		  (cont_eval)))                
	       ((backtrack)))))))
         
(defun load_PC ()
  (setq PC (pop CP) PCE (E CL) Cut_pt BL))
					; (if dbg (dbg PC) t))

(defun load_A (largs el)
  (dotimes (i (length largs) (vset Mem A i))
	   (vset Mem 
		 (+ A i 1) 
		 (let ((te (ultimate (pop largs) el)))
		   (cond
		    ((atom te) te)
		    ((var? te) (if (unsafe? te) (genvar (cdr te)) te))
                    ((copy? te) te)
		    ((recopy te el))))))) 

(defun unsafe? (x) (and (not CP) (>= (cdr x) CL)))

(defun pr2 (paq)
  (load_A (largs PC) PCE)
  (if CP 
      (pr paq)
    (progn
      (if (<= BL CL) (setq L CL))
      (setq CP (CP CL) CL (CL CL))
      (pr paq))))
        
(defun cont_eval ()
  (unless CP
	  (if (<= BL CL) (setq L CL)) (setq CP (CP CL) CL (CL CL))))

(defun pr (paq) 
  (if (cdr paq)       
      (progn (push_choix) (pr_choice paq))
    (pr_det (car paq))))

(defun pr_det (c)
  (if (eq (unify_with (largs (head c)) (push_E (nvar c))) 'fail)
      (backtrack)                
    (when (tail c) (push_cont) (setq CP (tail c) CL L) (maj_L (nvar c))))) 

(defun pr_choice (paq)
  (let* ((resu (shallow_backtrack paq)) (c (car resu)) (r (cdr resu)))
    (cond ((null r) (pop_choix) (pr_det c))
	  ( (push_bpr r)                   
	    (when (tail c)
		  (push_cont) 
		  (setq CP (tail c) CL L)
		  (maj_L (nvar c)))))))

(defun shallow_backtrack (paq)   
  (if (and (cdr paq) 
	   (eq (unify_with
		(largs (head (car paq)))
		(push_E (nvar (car paq)))) 'fail))
      (progn
	(poptrail (TR BL))
	(setq G BG)
	(shallow_backtrack (cdr paq)))
    paq))
              
(defun backtrack ()                        
  (if (zerop BL)                          
      (setq Duboulot nil)              
    (progn (setq L BL G BG Cut_pt (BL BL) CP (BCP L) CL (BCL L))
	   (load_A2)           
	   (poptrail (TR BL))          
	   (pr_choice (BP L)))))
   
(defun load_A2 ()
  (let ((deb (- L (size_C L))))
    (dotimes (i (A L) (vset Mem A i))
	     (vset Mem (+ A i 1) (svref Mem (+ deb i))))))

(defun myloop (c)
  (setq G BottomG L BottomL TR BottomTR 
        CP nil CL 0 BL 0 BG BottomG Duboulot t Cut_pt 0)
  (push_cont)
  (push_E (nvar c))
  (setq CP (cdr c) CL L)
  (maj_L (nvar c))  (read-char)
  (catch 'debord (forward))
  (myloop (read_prompt)))


; Version 2
; pred.lsp
;

(defvar Ob_Micro_Log 
      '(|write| |nl| |tab| |read| |get| |get0| 
	|var| |nonvar| |atomic| |atom| |number|
	! |fail| |true| 
	|divi| |mod| |plus| |minus| |mult| |le| |lt| 
	|name| |consult| |abolish| |cputime| |statistics|))
(mapc #' (lambda (x) (setf (get x 'evaluable) t)) Ob_Micro_Log)
  
(defmacro value (x)
     `(if (or (var? ,x) (atom ,x)) (ultimate ,x PCE) (copy ,x PCE)))
(defun uni (x y) (catch 'impossible (unif (value x) y)))
      
;write/1 (?term)
 (defun |write| (x) (write1 (value x)))
    (defun write1 (x)
      (cond 
       ((null x) (format t "[]"))
       ((atom x) (format t "~A" x))
       ((var? x) (format t "X~A" (cdr x)))
       ((list? x) (format t "[")
	(writesl (val (cadr x)) (val (caddr x)))
	(format t "]"))
       ((writesf (functor (des x)) (largs x)))))

    (defun writesl (tete q)
      (write1 tete)
      (cond
       ((null q))
       ((var? q) (format t "|X~A" (cdr q)))
       (t (format t ",") (writesl (val (cadr q)) (val (caddr q))))))

    (defun writesf (fct largs)
      (format t "~A(" fct)
      (write1 (val (car largs)))
      (mapc #' (lambda (x) (format t ",") (write1 (val x))) (cdr largs))
      (format t ")"))

 ;nl/0
       (defun |nl| () (terpri))
 ;tab/1 (+int)
       (defun |tab| (x) (dotimes (i (value x)) (format t " ")))
 ;read/1 (?term)
       (defun |read| (x) 
         (let ((te (read_terme)))
	   (catch 'impossible 
	     (unif (value x) (recopy (cdr te) (push_E (car te)))))))

        (defun read_terme ()
          (let ((*lvar nil))
            (let ((te (read_term (rchnsep))))
	      (rchnsep) (cons (length *lvar) te))))
 ;get/1 (?car)
       (defun |get| (x) (uni x (char-int (rchnsep))))
 ;get0/1 (?car)
       (defun |get0| (x) (uni x (char-int (read-char))))


 ;var/1 (?term)
        (defun |var| (x) (unless (var? (value x)) 'fail))
 ;nonvar/1 (?term)
        (defun |nonvar| (x) (if (var? (value x)) 'fail))
 ;atomic/1 (?term)
        (defun |atomic| (x) (if (listp (value x)) 'fail))
 ;atom/1 (?term)
        (defun |atom| (x) (unless (symbolp (value x)) 'fail))
 ;number/1 (?term)
        (defun |number| (x) (unless (numberp (value x)) 'fail))

 ;cut/0
       (defun ! (n) 
	 (setq BL (Cut CL) BG (if (zerop BL) BottomG (BG BL)) L (+ CL 3 n)))
 ;fail/0
       (defun |fail| () 'fail)
 ;true/0
       (defun |true| ())
 
 ;divi/3 (+int,+int,?int)
       (defun |divi| (x y z) (uni z (floor (value x) (value y))))
 ;mod/3 (+int,+int,?int)
       (defun |mod| (x y z) (uni z (rem (value x) (value y))))
 ;plus/3 (+int,+int,?int)
       (defun |plus| (x y z) (uni z (+ (value x) (value y))))
 ;minus/3 (+int,+int,?int)
       (defun |minus| (x y z) (uni z (- (value x) (value y))))
 ;mult/3 (+int,+int,?int)
       (defun |mult| (x y z) (uni z (* (value x) (value y))))
 ;le/2 (+int,+int)
       (defun |le| (x y) (if (> (value x) (value y)) 'fail))
 ;lt/2 (+int,+int)
       (defun |lt| (x y) (if (>= (value x) (value y)) 'fail))
 
;name/2 (?atom,?list)
(defun |name| (x y)
  (let ((b (value x)))
     (if (var? b) 
         (uni x (impl (undo_l (value y))))
         (uni y (do_l (expl b))))))
(defun undo_l (x) 
  (if (atom x) 
       x
      (cons (undo_l (val (cadr x))) (undo_l (val (caddr x))))))
(defun do_l (x) (if (atom x) x (list '(\. 2) (car x) (do_l (cdr x)))))
(defun impl (l) (intern (map 'string #'int-char l)))
(defun expl (at) (map 'list #'char-int (string at)))

 ;consult/1 (+atom)
      (defun |consult| (f) (format t "~A~%" (load (value f))))
					; abolish/1
(defun |abolish| (p)
  (mapc  #'(lambda (x) (setf (get p x) nil))
	 '(atom empty list fonct def)))
					; cputime/1
(defun |cputime| (x)
  (uni x (float (/ (get-internal-run-time) internal-time-units-per-second))))
					; statistics/0 
(defun |statistics| ()
  (format t " local stack : ~A (~A used)~%" (- BottomTR BottomL) (- L BottomL))
  (format t " global stack : ~A (~A used)~%" BottomL (- G BottomG))
  (format t " trail : ~A (~A used)~%" (- A BottomTR) (- TR BottomTR)))
