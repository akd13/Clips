(deffacts init
        (digit 0 1 2 3 4 5 6 7 8 9)
)
        
(defrule enumerate-all-letters
        (letters $? ?a $?)
        (digit $? ?d $?)
        =>
        (assert (enum ?a ?d))
)

; (deffunction test (?x) ?x)

(deftemplate previous_column
        (multislot letterarray)
        (multislot numberarray)
        (slot carryover)
        (slot place)
        (slot length)
)

(deftemplate terminated
        (multislot letterarray)
        (multislot numberarray)
        (slot length)
)

(defrule first-column
        (declare (salience 10))

        (enum ?op1 ?d1)
        (enum ?op2 ?d2)
        (enum ?res ?d3)
        (first ?f1 1)
        (second ?s1 1)
        (sum ?sum1 1)
        (test (eq ?f1 ?op1))
        (test (eq ?s1 ?op2))
        (test (eq ?sum1 ?res))
        (test (if (eq ?f1 ?s1)
                then
                (eq ?d1 ?d2)
                else
                (neq ?d1 ?d2)))
        (test (if (eq ?f1 ?sum1)
                then
                (eq ?d1 ?d3)
                else
                (neq ?d1 ?d3)))
        (test (if (eq ?s1 ?sum1)
                then
                (eq ?d2 ?d3)
                else
                (neq ?d2 ?d3)))
        (test (eq (mod (+ ?d1 ?d2) 10) ?d3))
        =>
        (bind ?c (div (+ ?d1 ?d2) 10))
        (assert (previous_column (letterarray ?f1 ?s1 ?sum1) (numberarray ?d1 ?d2 ?d3) (carryover ?c) (place 1) (length 3)))

)

(defrule middle-column
        (declare (salience 20))
        
        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l) )
        
        (enum ?op1 ?d1)
        (enum ?op2 ?d2)
        (enum ?res ?d3)

        (first ?fn ?p)
        (second ?sn ?p)
        (sum ?sumn ?p)

        (test (neq ?p 1))

        (last ?last)
        (test (neq ?last ?p))

        (test (eq ?p (+ ?place 1)))

        (test (eq ?fn ?op1))
        (test (eq ?sn ?op2))
        (test (eq ?sumn ?res))

        
        (test (eq (mod (+ ?d1 ?d2 ?c) 10) ?d3))   

        (test (if (eq ?fn ?sn)
                then
                (eq ?d1 ?d2)
                else
                (neq ?d1 ?d2)))

        (test (if (eq ?fn ?sumn)
                then
                (eq ?d1 ?d3)
                else
                (neq ?d1 ?d3)))

        (test (if (eq ?sn ?sumn)
                then
                (eq ?d2 ?d3)
                else
                (neq ?d2 ?d3))) 

        (test (eq (member$ ?fn ?la) (member$ ?d1 ?na)))
        (test (eq (member$ ?sn ?la) (member$ ?d2 ?na)))
        (test (eq (member$ ?sumn ?la) (member$ ?d3 ?na)))    
        =>     
        (retract ?previous_column)
        (bind ?c (div (+ ?d1 ?d2) 10))
        (assert (previous_column (letterarray ?fn ?sn ?sumn $?la) (numberarray ?d1 ?d2 ?d3 $?na) (carryover ?c) (place (+ ?place 1)) (length (+ 3 ?l))))
)

(defrule last-column
        (declare (salience 30))
        
        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l) )
        
        (enum ?op1 ?d1)
        (enum ?op2 ?d2)
        (enum ?res ?d3)

        (first ?fn ?p)
        (second ?sn ?p)
        (sum ?sumn ?p)

        (last ?length_all)
        (test (eq ?length_all ?p))


        (test (eq ?p (+ ?place 1)))

        (test (eq ?fn ?op1))
        (test (eq ?sn ?op2))
        (test (eq ?sumn ?res))

        
        (test (eq (+ ?d1 ?d2 ?c) ?d3))   

        (test (if (eq ?fn ?sn)
                then
                (eq ?d1 ?d2)
                else
                (neq ?d1 ?d2)))

        (test (if (eq ?fn ?sumn)
                then
                (eq ?d1 ?d3)
                else
                (neq ?d1 ?d3)))

        (test (if (eq ?sn ?sumn)
                then
                (eq ?d2 ?d3)
                else
                (neq ?d2 ?d3))) 

        (test (eq (member$ ?fn ?la) (member$ ?d1 ?na)))
        (test (eq (member$ ?sn ?la) (member$ ?d2 ?na)))
        (test (eq (member$ ?sumn ?la) (member$ ?d3 ?na)))

        =>     

        (retract ?previous_column)
        (assert (terminated (letterarray ?fn ?sn ?sumn $?la) (numberarray ?d1 ?d2 ?d3 $?na) (length (+ ?l 3))))

)

(defrule finish
        (declare (salience 70))
        ?terminated <- (terminated (letterarray $?la) (numberarray $?na) (length ?l))
        
        =>
        
        (printout t " This is what you came for " crlf)
        (loop-for-count (?cnt 1 ?l) do
                (printout t " Letter " (nth$ ?cnt ?la) " is " (nth$ ?cnt ?na) crlf)
        )

        (printout t crlf)

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
