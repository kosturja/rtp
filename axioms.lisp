(setq *Axioms* (list
'(or (not (mortal ?x)) (not (born ?x ?t1)) (not (gt ?t2 ?t1 150)) (dead ?x ?t2))
     '(human Marcus)
     '(pompeian Marcus)
     '(born Marcus 40)
     '(or (not (human ?x)) (mortal ?x))
     
     '(or (not (pompeian ?x)) (died ?x 79) (erupted volcano 79))
     ))
