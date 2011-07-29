; Mini-CProlog
; pred.lsp
;

(defvar Ob_Micro_Log			; list of builtins
  '(|write| |nl| |tab| |read| |get| |get0| 
    |var| |nonvar| |atomic| |atom| |number|
    ! |fail| |true| 
    |divi| |mod| |plus| |minus| |mult| |le| |lt| 
    |name| |consult| |abolish| |cputime| |statistics|))

(mapc					; tags the builtins
 #' (lambda (x) (setf (get x 'evaluable) t)) 
 Ob_Micro_Log)
  
;cut/0
(defun ! (n) 
  (unless (< BL CL)
	  (do () ((< BL CL)) (setq BL (BL BL)))
	  (setq BG (if (zerop BL) BottomG (G BL)) L (+ CL 3 n))))

; statistics/0 
(defun |statistics| ()
  (format t " local stack : ~A (~A used)~%" 
	  (- BottomTR BottomL) (- L BottomL))
  (format t " global stack : ~A (~A used)~%" 
	  BottomL (- G BottomG))
  (format t " trail : ~A (~A used)~%" 
	  (- TopTR BottomTR) (- TR BottomTR)))
