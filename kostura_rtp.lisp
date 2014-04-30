;; Resolution Theorem Prover
;; Joshua Kostura
;; University of Maine
;; COS 570 Spring 2014

(defvar *Clause_list* '())
(load "unify.lisp")
(load "axioms.lisp")
;(defvar *Axioms* nil)
(defvar *Proof_list* nil)
(setf *Proof_list* '())

;(setq *Axioms* (list
;'(or (not (mortal ?x)) (not (born ?x ?t1)) (not (gt ?t2 ?t1 150)) (dead ?x ?t2))
;     '(human Marcus)
 ;    '(pompeian Marcus)
  ;   '(born Marcus 40)
   ;  '(or (not (human ?x)) (mortal ?x))
    ; 
     ;'(or (not (pompeian ?x)) (died ?x 79) (erupted volcano 79))
     ;'(or (not (died ?x ?t1)) (not (gt ?t2 ?t1)) (dead ?x ?t2))
     ;'(human  ?x)
     ;))

(defun reset_clause_list ()
  (setf *Clause_list* '()))

;;Determines if a clause starts with an OR
(defun or_clause (clause)
  (if (equal (car clause) 'or) t
      nil))
(defun remove_and_or (clause)
  (if (or (equal (car clause) 'or) (equal (car clause) 'and)) (cdr clause)))


; Removes an unecessary "and" and "or" from a clause
; This is necessary one we start removing imbedded clauses
; to prove them.
(defun remove_and_or (clause)
  (cond
    ((or (equal (car clause) 'or) (equal (car clause) 'and))
     (cond
       ((= (length clause) 2)
	(return-from remove_and_or (cadr clause))))
     (t
      (return-from remove_and_or clause)))))


;Determines if a clause starts with an AND
(defun and_clause (clause)
  (if (equal (car clause) 'and) t
      nil))

;; Determines implies clause
(defun implies_clause (clause)
  (if (equal (car clause) 'implies) t
      nil))
; Determines if a clause is Existential 
(defun exist_clause (clause)
  (if (equal (car clause) 'exists) t
      nil))

; Determines if a clause is Universal
(defun forall (clause)
  (if (equal (car clause) 'forall) t
      nil))
(defun is_conj (clause)
  (if (or (equal clause 'or) (equal clause 'and)) t
      nil))
;;Determines if a clause starts with an NOT
(defun not_clause (clause)
  (if (equal (car clause) 'not) t
      nil))
; Determines if a clause is compound
; for example (or (not (dead marcus)) (alive marcus))
(defun is_compound (clause)
  (if (or (not_clause clause) (forall clause) (exist_clause clause)
	  (implies_clause clause) (and_clause clause) (or_clause clause)) t
	  nil))

; Determines is a clause is compound, except it doesn't count negated
; clauses, so (not (dead marcus)) is not a compound clause, but
; (or (not (dead marcus)) (alive marcus)) is considered compound.
(defun is_compound_not (clause)
  (if (or (forall clause) (exist_clause clause)
	  (implies_clause clause) (and_clause clause) (or_clause clause)) t
	  nil))

;;A function to negate a clause
;; Creates a negated clause if the clause passed is true
;; Creates a true clause if the clause passed is negative.
(defun negate (clause)
  (if (not_clause clause) (cadr clause)
      (append (list 'not) (list clause))))


(defun my_find_2 (clause axiom_list)
  ;(print "Number 2")
  (let ((cnt2 0))
    (loop
       for item in axiom_list
	 ;do (print item)
	 if (equal clause item)
	 do (progn
	      (print (format t " ~S was resolved with ~S from ~S"(negate clause) item axiom_list))
	      (return cnt2))
	 else do (progn
		   ;(print cnt)
		   (setf cnt2(+ cnt2 1))))))

(defun my_find (clause axiom_list)
  ;; expects the negated version of the clause you are trying to find
  ;; it negates the clause then searches the KB to find the resolvent
  ;;
  ;; returns the location of the resolvent in KB
  ;; Returns pair if the resolvent is part of a larger axiom
  ;; the first index is the axiom, the second index is the location in the axiom
  ;; since we add one to our counter before we look at an axiom, we must subtract one
  ;; form the first index just before we return.
  (let* ((cnt 0)
	(clause (negate clause)))
    (loop
       for item in axiom_list
	 do (setf cnt  (+ cnt 1))
	 if (is_compound item)
	     do (progn
		  (let ((inner (my_find_2 clause item)))
		    (if (not (null inner))
			(return-from my_find (list (- cnt 1) inner)))))
       else if (equal clause item)
	 do (progn
	      (print (format t "~S was resolved with ~S from axiom ~S from the knowledge base." (negate clause) clause cnt))
	      (return-from my_find  (list (- cnt 1) 0))))))


(defun resolve (clause axiom_list location)
  ; This function takes an clause, the KB and the location of
  ; the clause in the KB, we then return a proof_list with the
  ; results of resolving the clause.
  (let* ((first_index (car location))
	(whole_axiom (nth first_index axiom_list))
	 (clause (negate clause))
	 (temp_proof '()))
    (pop *Proof_list*)
    (cond
      ((not (is_compound whole_axiom))
       (cond
	 ((equal clause whole_axiom)
	  ;(print (format t "Resolved with single clause:~S"clause))
	  (return-from resolve '())))))
    (loop
       for item in whole_axiom
	 ;do (print (format t "Resolve loop:~S"item))
	 if (not (equal clause item))
	 do (progn
	      (push (pop whole_axiom) temp_proof)))
    (setf temp_proof (reverse temp_proof))
    ;(print temp_proof)
    (return-from resolve temp_proof)))


; This was my first attempt at a prove function, least to say it doesn't work
; But i used it for a basis for my second version below
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


;; Accepts the negate clause to be proven.
;; If you wanna prove (human marcus) then pass (not (human marcus))
(defun prove (clause axiom_list &optional prflist)
	(cond
	  ((null clause)
	   (print (format t "We proved it!"))
	   (return-from prove t))
	  (t
	   (print (format t "We are trying to prove ~S" clause))
	   (let ((prooflist prflist)
		 (new_clause '())
		 (newer_clause '())
		 (temp '())
		 (temp2 '()))
	     ;(print (format t "Clause:~S Prflist:~S" clause prflist))
	     (cond
	       ((and (not (null clause)) (not (null prooflist)))
		;(print (format t "There not null"))
		(setf prooflist (append (list 'or clause prooflist))))
	       (t
		(setf prooflist (append clause))))
	     (cond
	       ((is_compound_not prooflist)
		(setf temp (car (last prooflist)))
		(print (format t "We are trying to resolve :~S" temp))
		(setf temp2 (remove_and_or  (reverse (cdr (reverse prooflist))))))
	       (t
		(setf temp prooflist)))
	     ;(print (format t "The rest of prooflist:~S "temp2))
	     ;(print (format t "Temp before we resolve:~S" temp))
	     (setf newer_clause (resolve temp axiom_list (my_find temp axiom_list)))
	     (cond
	       ((not (null newer_clause))
		;(print (format t "Removeing things :~S"newer_clause))
		(setf new_clause (remove_and_or newer_clause))
		(cond
		  ((not (null temp2))
		   (setf new_clause (append (list 'or temp2 new_clause))))))
	       (t
		;(print (format t "Newer_clause was nil:~S" temp2))
		(setf new_clause  temp2)))
	     ;(print (format t "Newer clause: ~S" newer_clause))
	     ;(print (format t "New Clause: ~S" new_clause))
	     (prove new_clause axiom_list )))))