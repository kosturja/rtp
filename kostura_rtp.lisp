;; Resolution Theorem Prover
;; Joshua Kostura
;; University of Maine
;; COS 570 Spring 2014

(defvar *Clause_list* '())
(load "unify.lisp")

(defvar *Axioms*
  (progn
    (list
     '(human Marcus)
     '(pompeian Marcus)
     '(born Marcus 40)
     '(or (not (human ?x)) (mortal ?x))
     '(or (not (pompeian ?x)) (died ?x 79) (erupted volcano 79))
     '(or (not (mortal ?x)) (not (born ?x ?t1)) (not (gt ?t2 ?t1 150)) (dead ?x ?t2)))))

(defun reset_clause_list ()
  (setf *Clause_list* '()))

;;Determines if a clause starts with an OR
(defun or_clause (clause)
  (if (equal (car clause) 'or) t
      nil))

;Determines if a c clause starts with an AND
(defun and_clause (clause)
  (if (equal (car clause) 'and) t
      nil))

;; Determines implies clause
(defun implies_clause (clause)
  (if (equal (car clause) 'implies) t
      nil))
; Existential 
(defun exist_clause (clause)
  (if (equal (car clause) 'exists) t
      nil))

;Universal
(defun forall (clause)
  (if (equal (car clause) 'forall) t
      nil))


;;Determines if a clause starts with an NOT
(defun not_clause (clause)
  (if (equal (car clause) 'not) t
      nil))
(defun is_compound (clause)
  (if (or (not_clause clause) (forall clause) (exist_clause clause)
	  (implies_clause clause) (and_clause clause) (or_clause clause)) t
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

(defun my_find_2 (clause axiom_list)
  ;(print "Number 2")
  (let ((cnt 0))
    (loop
       for item in axiom_list
	 ;do (print item)
	 if (equal clause item)
	 do (progn
	      (return cnt))
	 else do (progn
		   ;(print cnt)
		   (setf cnt(+ cnt 1))))))

(defun my_find (clause axiom_list)
  ;returns the location of clause in axiom_list
  (let ((cnt '(0 0)))
    (loop
       for item in axiom_list
	 if (> (length item) 2)
	     do (progn
		  (if (is_compound clause)
		  do (progn
		      (let ((inner (my_find_2 clause item)))
			(print "inner:")
			(print inner)
			(if (not (null inner))
			    (setf (cdr cnt) inner)))
		      (print cnt))))
	 else if (equal clause item)
           do (return-from my_find  cnt)
	 else
           do(progn
	       (print "car add")
	       (print (car cnt))
	       (setf (car cnt)(+ (car cnt) 1))))))

(defun mydisassemble (clause)
  (loop
     for item in clause
       do (push item *Clause_list*)))

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




	       