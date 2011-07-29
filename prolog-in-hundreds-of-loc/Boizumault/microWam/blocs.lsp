; mini-Wam
; blocks.lsp
;

; I. Registers
;

(defconstant BottomG 0)			; bottom of the heap
(defconstant BottomL 2000)		; bottom of local stack
(defconstant BottomTR 8000)		; bottom of trail
(defconstant A 10000)			; registers Ai

(defvar Mem (make-array 10050 :initial-element 0))
(defvar TR)				; top of trail
(defvar L)				; top of local stack
(defvar G)				; top of heap
(defvar CP)				; current continuation
(defvar CL)
(defvar Cut_pt)				; specific cut
(defvar BL)				; last choice point
(defvar BG)
(defvar PC)				; current goal and its
(defvar PCE)				; environment
(defvar Duboulot)

(defmacro vset (v i x) `(setf (svref ,v ,i) ,x))
(defmacro tcons (des largs)		; tags instances
  `(cons (append ,des (list '#\*)) ,largs))
(defmacro des (te) `(car ,te))
(defmacro arity (des) `(cadr ,des))
(defmacro functor (des) `(car ,des))
(defmacro largs (x) `(cdr ,x))
(defmacro var? (x) `(and (consp ,x) (numberp (cdr ,x))))
(defmacro list? (x) `(eq (functor (des ,x)) '\.))

; II. Local Stack
;

;deterministic block [CL CP Cut E]
;
(defmacro CL (b) `(svref Mem ,b))
(defmacro CP (b) `(svref Mem (1+ ,b)))
(defmacro Cut (b) `(svref Mem (+ ,b 2)))
(defmacro E (b) `(+ ,b 3))            

(defmacro push_cont ()			; saves continuation
  `(progn (vset Mem L CL) (vset Mem (1+ L) CP)))
  
(defmacro push_E (n)			; allocates env of size n
  `(let ((top (+ L 3 ,n)))	
     (if (>= top BottomTR)
	 (throw 'debord (print "Local Stack Overflow")))
     (vset Mem (+ L 2) Cut_pt)		; saves cut point
     (dotimes (i ,n top) (vset Mem (decf top) (cons 'V top)))))
 
(defmacro maj_L (nl) `(incf L (+ 3 ,nl)))


;Rerun parts : [a1 .. an A BCP BCL BG BL BP TR]
;
(defmacro TR (b) `(svref Mem (1- ,b)))
(defmacro BP (b) `(svref Mem (- ,b 2)))
(defmacro BL (b) `(svref Mem (- ,b 3)))
(defmacro BG (b) `(svref Mem (- ,b 4)))
(defmacro BCL (b) `(svref Mem (- ,b 5)))
(defmacro BCP (b) `(svref Mem (- ,b 6)))
(defmacro A (b) `(svref Mem (- ,b 7)))

(defun save_args ()			; save args of current goal
  (dotimes (i (svref Mem A) (vset Mem (incf L i) i))
	   (vset Mem (+ L i) (svref Mem (+ A i 1)))))

(defun push_choix ()			; allocates a choice point
  (save_args)				; goal args
  (vset Mem (incf L) CP)		; continuation
  (vset Mem (incf L) CL)
  (vset Mem (incf L) G)			; top of heap
  (vset Mem (incf L) BL)		; last choice point
  (vset Mem (incf L 2) TR)		; top of trail
  (setq BL (incf L) BG G))		; new last choice point
        
(defun push_bpr (reste) (vset Mem (- BL 2) reste))
(defmacro size_C (b) `(+ 7 (A ,b)))    

(defun pop_choix () 
  (setq L (- BL (size_C BL))		; pops the block
	BL (BL BL)			; updates backtrack reg.
	BG (if (zerop BL) BottomG (BG BL))))

; III. Copy Stack (Heap)
;
(defmacro push_G (x)			; to push a term instance
  `(if (>= (incf G) BottomL) 
       (throw 'debord (print "Heap Overflow"))
     (vset Mem G ,x)))

(defmacro adr (v e) `(+ (cdr ,v) ,e))

(defun copy (x e)			; copy skeleton x in
  (cond					; environment e
   ((var? x) 
    (let ((te (ult (adr x e))))		; dereferencing
      (if (var? te) (genvar (cdr te)) te))) ; a new one
   ((atom x) x)
   ((tcons (des x) (cop (largs x) e))))) ; recursively

(defun cop (l e)			; copy while tagging
  (if l (cons (copy (car l) e) (cop (cdr l) e))))

(defmacro recopy (x e) `(push_G (copy ,x ,e)))

(defmacro copy? (te) `(cddr (des ,te))) ; instance ?

;IV. Trail
;

(defmacro pushtrail (x)
  `(cond ((>= TR A) (throw 'debord (print "Trail Overflow")))
	 ((vset Mem TR ,x) (incf TR))))  

(defmacro poptrail (top)	       
  `(do () ((= TR ,top))
       (let ((v (svref Mem (decf TR)) )) (vset Mem v (cons 'V v)))))
