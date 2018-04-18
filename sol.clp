(deffacts init
        (digit 0 1 2 3 4 5 6 7 8 9)
)
        
(defrule enumerate-all-letters
        (declare (salience 90))
        (letters $? ?a $?)
        (digit $? ?d $?)
        =>
        (assert (enum ?a ?d))
)

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

; (defrule one-column-equal
;         (declare (salience 10)) 
;         (first ?fn 1)
;         (second ?sn 1)
;         (sum ?sumn 1)

;         (result-length 1)
;         (operand-length 1)

;         (enum ?op1 ?d1)
;         (enum ?op2 ?d2)
;         (enum ?res ?d3)

;         ?current_count <- (count ?count)

;         (test (eq (+ ?d1 ?d2) ?d3))   

;         (test (eq ?fn ?op1))
;         (test (eq ?sn ?op2))
;         (test (eq ?sumn ?res))

;         (test (if (eq ?fn ?sn)
;                 then
;                 (eq ?d1 ?d2)
;                 else
;                 (neq ?d1 ?d2)))

;         (test (if (eq ?fn ?sumn)
;                 then
;                 (eq ?d1 ?d3)
;                 else
;                 (neq ?d1 ?d3)))

;         (test (if (eq ?sn ?sumn)
;                 then
;                 (eq ?d2 ?d3)
;                 else
;                 (neq ?d2 ?d3))) 

;         =>     

;         (retract ?current_count)
;         (assert (count (+ ?count 1)))
;         (assert (terminated (letterarray ?fn ?sn ?sumn) (numberarray ?d1 ?d2 ?d3) (length 3)))

; )

; (defrule one-column-inequal
;         (declare (salience 10))        
;         (first ?fn 1)
;         (second ?sn 1)
;         (sum ?sumn 1)
;         (sum ?sumn_1 2)

;         (result-length 2)
;         (operand-length 1)

;         (enum ?op1 ?d1)
;         (enum ?op2 ?d2)
;         (enum ?res ?d3)
;         (enum ?res_new ?d4)

;         ?current_count <- (count ?count)

;         (test (eq (+ ?d1 ?d2) ?d3))   

;         (test (eq ?fn ?op1))
;         (test (eq ?sn ?op2))
;         (test (eq ?sumn ?res))
;         (test (eq ?sumn_1 ?res_new))

;         (test (if (eq ?fn ?sn)
;                 then
;                 (eq ?d1 ?d2)
;                 else
;                 (neq ?d1 ?d2)))

;         (test (if (eq ?fn ?sumn)
;                 then
;                 (eq ?d1 ?d3)
;                 else
;                 (neq ?d1 ?d3)))

;         (test (if (eq ?fn ?sumn_1)
;                 then
;                 (eq ?d1 ?d4)
;                 else
;                 (neq ?d1 ?d4))) 

;         (test (if (eq ?sn ?sumn)
;                 then
;                 (eq ?d2 ?d3)
;                 else
;                 (neq ?d2 ?d3)))

;         (test (if (eq ?sn ?sumn_1)
;                 then
;                 (eq ?d2 ?d4)
;                 else
;                 (neq ?d2 ?d4))) 

;         (test (if (eq ?sumn ?sumn_1)
;                 then
;                 (eq ?d3 ?d4)
;                 else
;                 (neq ?d3 ?d4))) 

;         =>     

;         (retract ?current_count)
;         (assert (count (+ ?count 1)))
;         (assert (terminated (letterarray ?fn ?sn ?sumn ?sumn_1) (numberarray ?d1 ?d2 ?d3 ?d4) (length 4)))

; )


(defrule first-column
        (declare (salience 10))
        
        (first ?f1 1)
        (second ?s1 1)
        (sum ?sum1 1)
        
        (enum ?op1 ?d1)
        (enum ?op2 ?d2)
        (enum ?res ?d3)

        (test (eq (mod (+ ?d1 ?d2) 10) ?d3))

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
        
        =>

        (bind ?c (div (+ ?d1 ?d2) 10))
        (assert (previous_column (letterarray ?f1 ?s1 ?sum1) (numberarray ?d1 ?d2 ?d3) (carryover ?c) (place 1) (length 3)))

)

(defrule middle-column
        (declare (salience 20))
        
        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l) )
        
        (first ?fn ?p)
        (second ?sn ?p)
        (sum ?sumn ?p)

        (test (neq ?p 1))      

        (test (eq ?p (+ ?place 1)))

        (result-length ?result-length)
        (test (neq ?result-length ?p))    
        
        (enum ?op1 ?d1)
        (enum ?op2 ?d2)
        (enum ?res ?d3)

        (test (eq (mod (+ ?d1 ?d2 ?c) 10) ?d3))   

        (test (eq ?fn ?op1))
        (test (eq ?sn ?op2))
        (test (eq ?sumn ?res))

        (test (eq (member$ ?fn ?la) (member$ ?d1 ?na)))
        (test (eq (member$ ?sn ?la) (member$ ?d2 ?na)))
        (test (eq (member$ ?sumn ?la) (member$ ?d3 ?na)))    
     
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
       
        =>     
       
        (retract ?previous_column)
        (bind ?c (div (+ ?d1 ?d2) 10))
        (assert (previous_column (letterarray ?fn ?sn ?sumn $?la) (numberarray ?d1 ?d2 ?d3 $?na) (carryover ?c) (place (+ ?place 1)) (length (+ 3 ?l))))
)

(defrule result-length-column-equal-length
        
        (declare (salience 30))

        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l) )
        
        (first ?fn ?p)
        (second ?sn ?p)
        (sum ?sumn ?p)

        (result-length ?length_all)
        (test (eq ?length_all ?p))

        (enum ?op1 ?d1)
        (enum ?op2 ?d2)
        (enum ?res ?d3)

        ?current_count <- (count ?count)

        (test (eq (+ ?d1 ?d2 ?c) ?d3))   

        (test (eq ?p (+ ?place 1)))

        (test (eq ?fn ?op1))
        (test (eq ?sn ?op2))
        (test (eq ?sumn ?res))

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

        (retract ?current_count)
        (assert (count (+ ?count 1)))
        (retract ?previous_column)
        (assert (terminated (letterarray ?fn ?sn ?sumn $?la) (numberarray ?d1 ?d2 ?d3 $?na) (length (+ ?l 3))))

)

(defrule result-length-column-inequal-length
        
        (declare (salience 30))

        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l) )
        
        (first ?fn ?p)
        (second ?sn ?p)
        (sum ?sumn ?p)

        (result-length ?length_all)
        (sum ?sumn_1 ?length_all)

        (test (eq ?length_all (+ ?p 1)))

        (enum ?op1 ?d1)
        (enum ?op2 ?d2)
        (enum ?res ?d3)
        (enum ?res_new ?d4)

        (test (or (eq ?d4 1) (eq ?d4 0)))


        ?current_count <- (count ?count)

        (test (eq (mod (+ ?d1 ?d2 ?c) 10) ?d3))
        (test (eq (div (+ ?d1 ?d2 ?c) 10) ?d4))  

        (test (eq ?p (+ ?place 1)))

        (test (eq ?fn ?op1))
        (test (eq ?sn ?op2))
        (test (eq ?sumn ?res))
        (test (eq ?sumn_1 ?res_new))

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

        (test (if (eq ?fn ?sumn_1)
                then
                (eq ?d1 ?d4)
                else
                (neq ?d1 ?d4))) 

        (test (if (eq ?sn ?sumn)
                then
                (eq ?d2 ?d3)
                else
                (neq ?d2 ?d3)))

        (test (if (eq ?sn ?sumn_1)
                then
                (eq ?d2 ?d4)
                else
                (neq ?d2 ?d4))) 

        (test (if (eq ?sumn ?sumn_1)
                then
                (eq ?d3 ?d4)
                else
                (neq ?d3 ?d4))) 

        (test (eq (member$ ?fn ?la) (member$ ?d1 ?na)))
        (test (eq (member$ ?sn ?la) (member$ ?d2 ?na)))
        (test (eq (member$ ?sumn ?la) (member$ ?d3 ?na)))
        (test (eq (member$ ?sumn_1 ?la) (member$ ?d4 ?na)))


        =>     

        (retract ?current_count)
        (assert (count (+ ?count 1)))
        (retract ?previous_column)
        (assert (terminated (letterarray ?fn ?sn ?sumn ?sumn_1 $?la) (numberarray ?d1 ?d2 ?d3 ?d4 $?na) (length (+ ?l 4))))

)


(defrule finish
        (declare (salience 70))
        ?terminated <- (terminated (letterarray $?la) (numberarray $?na) (length ?l))
        (count ?count)
        
        =>
        
        (printout t " One answer is " crlf)
        (loop-for-count (?cnt 1 ?l) do
                (printout t " Letter " (nth$ ?cnt ?la) " is " (nth$ ?cnt ?na) crlf)
        )

        (printout t " Solution(s): " ?count crlf)
        (printout t crlf)

)

(defrule retract-redundant
        (declare (salience 100))
        (operand-length ?oplength)
        (result-length ?reslength)
        (test (eq ?reslength (+ 1 ?oplength)))
        (sum ?reschar ?reslength)
        ?fact <- (enum ?reschar ?d)
        (test (neq ?d 0))
        (test (neq ?d 1))
        =>
        (retract ?fact)

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

        (bind ?oplength (length$ (create$ ?op1array)))
        (bind ?reslength (length$ (create$ ?resarray)))
        (printout t "Length of operands is " ?oplength  " & result is " ?reslength crlf)
        
        (assert (operand-length ?oplength))
        (assert (result-length ?reslength))
        
        (loop-for-count (?cnt 1 ?oplength) do
                (bind ?d (+ (- ?oplength ?cnt) 1))
                (bind ?op1char (nth$ ?d ?op1array))
                (assert (first ?op1char ?cnt))
        )

        (loop-for-count (?cnt 1 ?oplength) do
                (bind ?d (+ (- ?oplength ?cnt) 1))
                (bind ?op2char (nth$ ?d ?op2array))
                (assert (second ?op2char ?cnt))
        )

        (loop-for-count (?cnt 1 ?reslength) do
                (bind ?d (+ (- ?reslength ?cnt) 1))
                (bind ?reschar (nth$ ?d ?resarray))
                (assert (sum ?reschar ?cnt))
        )


        (assert (count 0))
)