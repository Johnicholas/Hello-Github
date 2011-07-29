; Mini-Cprolog
; parser.lsp
;

(defun read_code_cl ()                  ; to read and code a clause
  (let ((*lvarloc ()) (*lvarglob ()))
    (let ((x (read_clause (rchnsep))))
      (cons (cons (length *lvarloc)     ; number of local vars
                  (length *lvarglob))   ; number of global vars
            (c x)))))
              
(defun read_code_tail ()                ; idem for a query
  (setq *lvarloc () *lvarglob ())
  (let ((x (read_tail (rchnsep))))
    (cons (cons (length *lvarloc) (length *lvarglob)) (c x))))

(defun read_pred (ch)                   ; to read a literal
  (let ((nom (read_atom ch)) (c (rchnsep)))
    (if (char= c #\()
        (cons nom (read_args (rchnsep) 1)) 
      (progn (unread-char c) (list nom)))))
