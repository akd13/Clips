(deffacts init
	(digit 0 1 2 3 4 5 6 7 8 9)
)
	
(defrule enumerate-all-letters
	(letters $? ?a $?)
	(digit $? ?d $?)
	=>
	(assert (enum ?a ?d))
)

(defrule first-column
        (declare (salience 10))
	(enum ?op1 ?d1)
	(enum ?op2 ?d2&~?d1)
	(enum ?res ?d3&~?d2&~?d1)
	(first ?f1 1)
	(second ?s1 1)
	(sum ?sum 1)
        (test (eq ?f1 ?op1))
        (test (eq ?s1 ?op2))
        (test (eq ?sum ?res))
	(test (eq (mod (+ ?d1 ?d2) 10) ?d3))
        =>
        (bind ?c (div (+ ?d1 ?d2) 10))
        (assert (carryover ?c))
        (assert (assigned ?d1 ?d2 ?d3))
        (assert (input ?f1 ?s1 ?sum))
)

(defrule middle-column
        (declare (salience 30))
        ?carry <- (carryover ?c)
        (enum ?op1 ?d1)
        (enum ?op2 ?d2&~?d1)
        (enum ?res ?d3&~?d2&~?d1)
        ?input <-  (input ?f11 ?f22 ?f33)
        ?assign <- (assigned ?a1 ?a2 ?a3)     
        (first ?f1 ?p)
        (second ?s1 ?p)
        (sum ?sum ?p)
        (test (neq ?p 1))
        (test (neq ?d1 ?a1 ?a2 ?a3))
        (test (neq ?d2 ?a1 ?a2 ?a3))
        (test (neq ?d3 ?a1 ?a2 ?a3))
        (test (eq ?f1 ?op1))
        (test (eq ?s1 ?op2))
        (test (eq ?sum ?res))
        (test (eq (mod (+ ?d1 ?d2 ?c) 10) ?d3))
        (last ?r)
        (test (neq ?r ?p))
        =>
        (bind ?c (div (+ ?d1 ?d2 ?c) 10))
        (assert (carryover1 ?c))
        (assert (assigned ?d1 ?d2 ?d3 ?a1 ?a2 ?a3))
        (assert (input ?f1 ?s1 ?sum ?f11 ?f22 ?f33))
        (retract ?carry)
        (retract ?assign)
        (retract ?input)
        (retract ?assign)
)

(defrule last-column
        (declare (salience 50))
        ?carry <- (carryover1 ?c)
        (enum ?op1 ?d1)
        (enum ?op2 ?d2&~?d1)
        (enum ?res ?d3&~?d2&~?d1)
        ?assign <- (assigned ?a1 ?a2 ?a3 ?b1 ?b2 ?b3)
        ?input <-(input ?f11 ?f22 ?f33 ?f111 ?f222 ?f333)
        (first ?f1 ?p)
        (second ?s1 ?p)
        (sum ?sum ?p)
        (last ?r)
        (test (eq ?r ?p))
        (test (neq ?d1 ?a1 ?a2 ?a3 ?b1 ?b2 ?b3))
        (test (neq ?d2 ?a1 ?a2 ?a3 ?b1 ?b2 ?b3))
        (test (neq ?d3 ?a1 ?a2 ?a3 ?b1 ?b2 ?b3))
        (test (eq ?f1 ?op1))
        (test (eq ?s1 ?op2))
        (test (eq ?sum ?res))
        (test (eq (+ ?d1 ?d2 ?c) ?d3))
        =>
        (retract ?carry)
        (retract ?input)
        (retract ?assign)
        (assert (input ?f1 ?s1 ?sum ?f11 ?f22 ?f33 ?f111 ?f222 ?f333))
        (assert (assigned ?d1 ?d2 ?d3 ?a1 ?a2 ?a3 ?b1 ?b2 ?b3))
)

(defrule terminated
        (declare (salience 100))
        (input ?f1 ?f2 ?f3 ?f11 ?f22 ?f33 ?f111 ?f222 ?f333)
        (assigned ?d1 ?d2 ?d3 ?a1 ?a2 ?a3 ?b1 ?b2 ?b3)
        =>
        (printout t "First row is " ?f1 ?f11 ?f111 "-"?d1 ?a1 ?b1 crlf)
        (printout t "Second row is " ?f2 ?f22 ?f222 "-" ?d2 ?a2 ?b2 crlf)
        (printout t "Third row is " ?f3 ?f33 ?f333 "-" ?d3 ?a3 ?b3 crlf)

)

(defrule input
        (declare (salience 5))
	=>
	(printout t "op1:")
	(bind ?op1 (readline))
	(printout t "op2:")
	(bind ?op2 (readline))
	(printout t "res:")
	(bind ?res (readline))
	(bind ?op1array (explode$ ?op1))
	(bind ?op2array (explode$ ?op2))
	(bind ?resarray (explode$ ?res))
	(bind ?all_letters ?op1array ?op2array ?resarray)
	(assert (letters ?all_letters))
        (bind ?reslength (length$ (create$ ?resarray)))
        (printout t "Length is " ?reslength crlf)

        (loop-for-count (?cnt 1 ?reslength) do
                (bind ?d (+ (- ?reslength ?cnt) 1))
                (bind ?op1char (nth$ ?d ?op1array))
                (assert (first ?op1char ?cnt))
        )

        (loop-for-count (?cnt 1 ?reslength) do
                (bind ?d (+ (- ?reslength ?cnt) 1))
                (bind ?op2char (nth$ ?d ?op2array))
                (assert (second ?op2char ?cnt))
        )

        (loop-for-count (?cnt 1 ?reslength) do
                (bind ?d (+ (- ?reslength ?cnt) 1))
                (bind ?reschar (nth$ ?d ?resarray))
                (assert (sum ?reschar ?cnt))
        )

        (assert (last ?reslength))
)
