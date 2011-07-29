; mini-Cprolog
; blocks.lsp
;

; I. Registers
;
(defconstant BottomG 1)                 ; bottom of global stack
(defconstant BottomL 3000)              ; bottom of local stack
(defconstant BottomTR 8000)             ; bottom of trail
(defconstant TopTR 10000)               ; max of trail

(defvar Mem (make-array 10000 :initial-element 0))
(defvar TR)                             ; top of trail
(defvar L)                              ; top of local stack
(defvar G)                              ; top of global stack
(defvar CP)                             ; current continuation
(defvar CL)
(defvar BL)                             ; last choice point
(defvar BG)
(defvar PC)                             ; current goal and its
(defvar PCE)                            ; environments
(defvar PCG)
(defvar Duboulot)

(defmacro vset (v i x) `(setf (svref ,v ,i) ,x))
            
; II. Local Stack 
; deterministic block : [CL CP E G]
;
(defmacro CL (b) `(svref Mem ,b))
(defmacro CP (b) `(svref Mem (1+ ,b)))
(defmacro G (b) `(svref Mem(+ ,b 2)))
(defmacro E (b) `(+ ,b 3))

(defmacro push_cont ()                  ; saves continuation
  `(progn (vset Mem L CL) (vset Mem (1+ L) CP)))
  
(defmacro push_E (n)                    ; allocates local env.
  `(let ((top (+ L 3 ,n)))
     (if (>= top BottomTR) 
         (throw 'debord (print "Local Stack Overflow")))
     (dotimes (i ,n top)                ; n local free variables
       (vset Mem (decf top) (cons 'LIBRE BottomG)))))
 
(defmacro maj_L (nl)                    ; updates top of local stack
  `(incf L (+ 3 ,nl)))

; choice block : [TR BP BL] 
;
(defmacro BL (b) `(svref Mem (1- ,b)))
(defmacro BP (b) `(svref Mem (- ,b 2)))
(defmacro TR (b) `(svref Mem (- ,b 3)))
  
(defmacro push_choix ()                 ; allocates a rerun part
  `(progn (vset Mem L TR)               ; current top of trail
          (vset Mem (incf L 2) BL)      ; saves last choice block
          (setq BL (incf L) BG G)))     ; new last choice block

(defmacro push_bpr (reste)              ; subset of clauses still
  `(vset Mem (- BL 2) ,reste))          ; to be considered

(defmacro pop_choix ()
  `(setq L (- L 3)                      ; pops the block
         BL (BL BL)                     ; updates backtrack
         BG (if (zerop BL) 0 (G BL))))  ; specific registers

; III. Global Stack 
;
(defmacro push_G (n)                    ; allocates a global env
  `(let ((top (+ G ,n)))
     (if (>= top BottomL) 
         (throw 'debord (print "Global Stack Overflow")))
     (dotimes (i ,n (vset Mem (+ L 2) G)) ; field G of local stack
       (vset Mem (decf top) (cons 'LIBRE BottomG)))))

(defmacro maj_G (ng) `(incf G ,ng))

; IV. Trail
;
(defmacro pushtrail (x)                 ; remembers that x
  `(progn                               ; is post-bound
     (if (>= TR TopTR) (throw 'debord (print "Trail Overflow")))
     (vset Mem TR ,x)
     (incf TR)))  

(defmacro poptrail (top)                ; pops the trail until top
  `(do () ((= TR ,top))                 ; restore vars to free state
     (vset Mem (svref Mem (decf TR)) (cons 'LIBRE BottomG))))
