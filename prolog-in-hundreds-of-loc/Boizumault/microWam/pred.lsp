; mini-Wam
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
	 (setq BL (Cut CL)
	       BG (if (zerop BL) BottomG (BG BL))
	       L (+ CL 3 n)))		; updates local stack
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
(defun do_l (x) 
  (if (atom x) x (list '(\. 2) (car x) (do_l (cdr x)))))
(defun impl (l) (intern (map 'string #'int-char l)))
(defun expl (at) (map 'list #'char-int (string at)))

 ;consult/1 (+atom)
      (defun |consult| (f) (format t "~A~%" (load (value f))))
; abolish/1 (+atom)
(defun |abolish| (p)
  (mapc  #'(lambda (x) (setf (get p x) nil))
	 '(atom empty list fonct def)))

; cputime/1 (?int)
(defun |cputime| (x)
  (uni x (float (/ (get-internal-run-time) 
		   internal-time-units-per-second))))

; statistics/0 
(defun |statistics| ()
  (format t " local stack : ~A (~A used)~%" 
	  (- BottomTR BottomL) (- L BottomL))
  (format t " global stack : ~A (~A used)~%" 
	  BottomL (- G BottomG))
  (format t " trail : ~A (~A used)~%" 
	  (- A BottomTR) (- TR BottomTR)))
