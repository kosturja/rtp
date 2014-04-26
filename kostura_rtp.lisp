;; Resolution Theorem Prover
;; Joshua Kostura
;; University of Maine
;; COS 570 Spring 2014

(defvar *Clause_list*)

(defun myfun ()
  (+ 5 5))

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
      (let (item cadr clause)
	(loop for item in clause)
		 (push item *Clause_list*))))


	       