; mini-Wam
; boot.lsp
;
  
(defun mini-Wam ()
  (banner)
  (format t "~A~%" (load "../microUt/mlg.Start"))
  (myloop (read_prompt)))

(defun read_prompt () 
  (terpri)
  (format t "| ?- ")
  (read_code_tail))

(defun banner ()
  (dotimes (i 2) (terpri))
  (format t "mini-Wam~%")
  (dotimes (i 2) (terpri)))

(defun l ()
  (format t "Back to mini-Wam top-level~%")
  (myloop (read_prompt)))
