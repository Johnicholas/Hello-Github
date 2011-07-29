; mini-Cprolog
; boot.lsp
;
  
(defun mini-Cprolog ()                  ; to run it
  (banner)
  (format t "~A~%" (load "../microUt/mlg.Start"))
  (myloop (read_prompt)))

(defun read_prompt () 
  (terpri)
  (format t "| ?- ")
  (read_code_tail))

(defun banner ()
  (dotimes (i 2) (terpri))
  (format t "Mini-Cprolog~%")
  (dotimes (i 2) (terpri)))

(defun l ()
  (format t "Back to Mini-Cprolog~%")   ; to re-enter from Lisp
  (myloop (read_prompt)))
