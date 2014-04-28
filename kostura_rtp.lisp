;; Resolution Theorem Prover
;; Joshua Kostura
;; University of Maine
;; COS 570 Spring 2014

(defvar *Clause_list* '())
(load "unify.lisp")

(defvar *Axioms* nil)
(defvar *Proof_list* nil)
(setf *Proof_list* '())

(setq *Axioms* (list
'(or (not (mortal ?x)) (not (born ?x ?t1)) (not (gt ?t2 ?t1 150)) (dead ?x ?t2))
     '(human Marcus)
     '(pompeian Marcus)
     '(born Marcus 40)
     '(or (not (human ?x)) (mortal ?x))
     
     '(or (not (pompeian ?x)) (died ?x 79) (erupted volcano 79))
     '(or (not (died ?x ?t1)) (not (gt ?t2 ?t1)) (dead ?x ?t2))
     ))

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
  (if (not_clause clause) (cadr clause)
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
  (let ((cnt2 0))
    (loop
       for item in axiom_list
	 ;do (print item)
	 if (equal clause item)
	 do (progn
	      (return cnt2))
	 else do (progn
		   ;(print cnt)
		   (setf cnt2(+ cnt2 1))))))

(defun my_find (clause axiom_list)
  ;returns the location of clause in axiom_list
  ; includes imbedded list, in which case it returns a pair
  ; (index in axiom set, index in to specific axiom)
  ; since we add one to our counter before we look at an axiom, we must subtract one
  ; form the first index just before we return.
  (let ((cnt 0))
    (loop
       for item in axiom_list
	 do (setf cnt  (+ cnt 1))
	 if (> (length item) 3)
	     do (progn
		  (let ((inner (my_find_2 clause item)))
		    (if (not (null inner))
			(return-from my_find (list (- cnt 1) inner)))))
       else if (equal clause item)
       do (return-from my_find  cnt))))

(defun mydisassemble (clause)
  (loop
     for item in clause
       do (push item *Clause_list*)))

(defun resolve (clause axiom_list location)
  ; This function takes an clause, the KB and the location of
  ; the clause in the KB, we then return a proof_list with the
  ; results of resolving the clause.
  (let* ((first_index (car location))
	(second_index (cadr location))
	(whole_axiom (nth first_index *axioms*))
	 (temp_proof *Proof_list*))
    (pop *Proof_list*)
    (loop
       for item in whole_axiom
	 ;do (print "Looping")
	 if (not (equal clause item))
	 do (progn
	      (push (pop whole_axiom) temp_proof)))
    (setf temp_proof (reverse temp_proof))
    (print temp_proof)
    (return-from resolve temp_proof)))



(defun prove1(clause axiom_list)
  (print (format t "We are trying to prove that ~S" clause))
  (let ((new_clause (negate clause))
	(prooflist '())
	(newer_clause '()))
    (push new_clause prooflist); The thing we want to prove, negated clause.
    ; Add it to the proof list
    (loop
       for item in prooflist
	 do (print "---------")
	 do (print prooflist)
	 do (print "---------")
	 do (print item)
	 do (print "==========")
	 do (setf newest_clause item)
					; Pick the first clause to resolve from proof_list
					; Then we negate it and see if it resides in the axioms
	 do (setf location (my_find (negate newest_clause) *axioms*))
	 if (not (null location))
	 do (setf newer_clause (resolve (negate newest_clause) axiom_list location))
	 ;do (print newer_clause)
	 do (setf prooflist newer_clause)
	 do (print prooflist)
	 )))
					; store its location in the axiom set in location

	
(defun prove (clause axiom_list &optional prflist)
  (print (format t "We are trying to prove that ~S" clause))
  (let ((prooflist prflist)
	(new_clause (negate clause))
	(newer_clause '())
	(temp '()))
    (cond
      ((is_compound clause)
       (loop
	  for item in clause
	    do (push item temp))
       (setf temp (reverse temp)))
      (t
       (setf temp clause)))
      
 ;   (if (is_compound clause)
;	(progn
;	  (loop
;	     for item in clause
;	     do (push item temp)))
;	(setf temp (reverse temp))
	;only reverse if we parse clause otherwisejust append clause
	;(setf temp clause))
    ;(setf temp (reverse temp))
    (if (and (not (null temp)) (not (null prooflist)))
	(setf prooflist (append (list 'or temp prooflist)))
	(setf prooflist (append  clause)))
    (print "Final List")
    (print prooflist)))
    