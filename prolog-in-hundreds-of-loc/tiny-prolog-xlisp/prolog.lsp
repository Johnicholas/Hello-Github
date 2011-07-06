;; The following is a tiny Prolog interpreter in MacLisp
;; written by Ken Kahn and modified for XLISP by David Betz.
;; (slightly modified again by Johnicholas Hines for XLISP 3.3;
;; some drift between versions? need to load clisp.lsp first?)
;; It was inspired by other tiny Lisp-based Prologs of
;; Par Emanuelson and Martin Nilsson.
;; There are no side-effects anywhere in the implementation.
;; Though it is VERY slow of course.

(define prolog (lambda (database &aux goal)
       (do () ((not (begin (display "Query? ") (set! goal (read)) (display goal))))
              (prove (list (rename-variables goal '(0)))
                     '((bottom-of-environment))
                     database
                     1))))

;; prove - proves the conjunction of the list-of-goals
;;         in the current environment

(define prove (lambda (list-of-goals environment database level)
      (cond ((null? list-of-goals) ;; succeeded since there are no goals
             (print-bindings environment environment)
             (not (y-or-n-p "More?")))
            (t (try-each database database
                         (cdr list-of-goals) (car list-of-goals)
                         environment level)))))

(define try-each (lambda (database-left database goals-left goal environment level 
                 &aux assertion new-enviroment)
       (cond ((null? database-left) nil) ;; fail since nothing left in database
             (t (set! assertion
                      (rename-variables (car database-left)
                                        (list level)))
                (set! new-environment
                      (unify goal (car assertion) environment))
                (cond ((null? new-environment) ;; failed to unify
                       (try-each (cdr database-left) database
                                 goals-left goal
                                 environment level))
                      ((prove (append (cdr assertion) goals-left)
                              new-environment
                              database
                              (+ 1 level)))
                      (t (try-each (cdr database-left) database
                                   goals-left goal
                                   environment level)))))))

(define unify (lambda (x y environment &aux new-environment)
       (set! x (value x environment))
       (set! y (value y environment))
       (cond ((variable-p x) (cons (list x y) environment))
             ((variable-p y) (cons (list y x) environment))
             ((or (atom? x) (atom? y))
                  (cond ((equal? x y) environment)
    	                (t nil)))
             (t (set! new-environment (unify (car x) (car y) environment))
                (cond (new-environment (unify (cdr x) (cdr y) new-environment))
    		      (t nil))))))

(define value (lambda (x environment &aux binding)
       (cond ((variable-p x)
              (set! binding (assoc x environment :test #'equal?))
              (cond ((null? binding) x)
                    (t (value (cadr binding) environment))))
             (t x))))

(define variable-p (lambda (x)
       (and x (list? x) (eq? (car x) '?))))

(define rename-variables (lambda (term list-of-level)
       (cond ((variable-p term) (append term list-of-level))
             ((atom? term) term)
             (t (cons (rename-variables (car term) list-of-level)
                      (rename-variables (cdr term) list-of-level))))))

(define print-bindings (lambda (environment-left environment)
       (cond ((cdr environment-left)
              (cond ((= 0 (list-ref 2 (caar environment-left)))
                     (write (cadr (caar environment-left)))
                     (display " = ")
                     (print (value (caar environment-left) environment))))
              (print-bindings (cdr environment-left) environment)))))

;; a sample database:
(define db '(((father madelyn ernest))
	     ((mother madelyn virginia))
	     ((father david arnold))
	     ((mother david pauline))
	     ((father rachel david))
	     ((mother rachel madelyn))
	     ((grandparent (? grandparent) (? grandchild))
	      (parent (? grandparent) (? parent))
	      (parent (? parent) (? grandchild)))
	     ((parent (? parent) (? child))
	      (mother (? parent) (? child)))
	     ((parent (? parent) (? child))
	      (father (? parent) (? child)))))

;; the following are utilities
(define y-or-n-p (lambda (prompt)
       (print prompt)
       (eq? (read) 'y)))

;; start things going
(prolog db)
