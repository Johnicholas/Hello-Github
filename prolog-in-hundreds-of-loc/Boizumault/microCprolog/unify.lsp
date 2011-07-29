; mini-Cprolog
; unify.lsp
;

(defmacro bindte (x sq e)               ; binds var x to
  `(progn                               ; the pair (sq,e)
     (if (or (and (> ,x BottomL)        ; local var post-bound?
                  (< ,x BL))
             (< ,x BG))                 ; global var post-bound ?
         (pushtrail ,x))                ; push it on the trail
     (rplaca (svref Mem ,x) ,sq)        ; binds it
     (rplacd (svref Mem ,x) ,e))) 
 
(defun unify (t1 el1 eg1 t2 el2 eg2)    ; unifies 2 lists of args
  (catch 'impossible  
    (do () ((null t1))
      (unif (ultimate (pop t1) el1 eg1) 
            (ultimate (pop t2) el2 eg2)))))
