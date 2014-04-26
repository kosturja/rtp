;; Resolution Theorem Prover
;; Joshua Kostura
;; University of Maine
;; COS 570 Spring 2014

(defvar *Clause_list* '())
(load "unify.lisp")
(defun reset_clause_list ()
  (setf *Clause_list* '()))

;;Determines if a clause starts with an OR
(defun or_clause (clause)
  (if (equal (car clause) 'or) t
      nil))

;;Determines if a clause starts with an NOT
(defun not_clause (clause)
  (if (equal (car clause) 'not) t
      nil))


;;A function to negate a clause
;; Creates a negated clause if the clause passed is true
;; Creates a true clause if the clause passed is negative.
(defun negate (clause)
  (if (not_clause clause) (cdr clause)
      (append (list 'not) (list clause))))


(defun handle_or_clause (clause)
  ;;Designed to take an OR clause, and seperate each clause
  (if (not (or_clause clause)) "Error No OR detected"
      (let ((clause (cdr clause)))
	(loop
	     for item in clause
	     do (push item *Clause_list*))
	(print *Clause_list*))))



(defun my_find (clause axiom_list)
  ;returns the location of clause in axiom_list
  (if (not (member clause axiom_list :test 'equal)) (print "Clause not found")
      (let ((cnt 0))
	(loop
	     for item in axiom_list
	     if (equal clause item)
	     do (return-from my_find  cnt)
	     else do(setf cnt(+ cnt 1))))))



(defun resolve (clause axiom_list)
  ; First check it we have negated clause, then remove the 'NOT and check to see if the positive 
  ; is in the axiom list.
  (if (not_clause clause)
      (let ((clause (cadr clause)))
	(setf location (my_find clause axiom_list))
	(member clause axiom_list :test 'equal)
	(return-from resolve location))
      ;clause is positive so we negate it and see that is in the axiom list
      (let ((clause (negate clause)))
	(setf location (my_find clause axiom_list))
	(member clause axiom_list :test 'equal)
	(return-from resolve location))))




	       