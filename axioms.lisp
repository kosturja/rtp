(defvar *axioms* nil)

(setq *Axioms* (list
		'(pompeian marcus)
		; pompeian(marcus), marcus is pompeian
		'(born marcus 40)
		; born (marcus,40), marcus was born in 40
		'(or (not (pompeian marcus)) (not (born marcus 40)) (human marcus))
		; pompeian(marcus) ^ born (marcus,40 implies human(marcus
		; If marcus is pompeian and was born in 40 then he is human
		))

(defvar *axiom* nil)

(setq *axiom* (list
	       ; pompeian(marcus), marcus is pompeian
	       '(pompeian marcus)
	       ; born(marcus,40), marcus was born in 40
	       '(born marcus 40)
	       ; if marcus is pompeian and born in 40 then he is mortal
	       ; pompeian(marcus) and born(marcus,40) implies mortal(marcus,now)
	       '(or (not (pompeian marcus)) (not (born marcus 40)) (mortal marcus))
	       ; if marcus is mortal implies he is dead now
	       ; (mortal marcus) implies (dead marcus now)
	       '(or (not (mortal marcus)) (not (born marcus 40)) (dead marcus))
	       ))


