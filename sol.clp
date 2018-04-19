(deffacts init                          ; initialize digits for letters
        (digit 0 1 2 3 4 5 6 7 8 9) 
)
        
(defrule enumerate-all-letters          ; enumerate all letters first
        (declare (salience 90)) 
        (letters $? ?a $?)
        (digit $? ?d $?)
        =>
        (assert (enum ?a ?d))
)

(defrule retract-redundant-res-inequal  ; retract all numbers except 1 from the residual result letter (e.g OH+NO=LOL will have L=1)
        (declare (salience 100))
        (operand-length ?oplength)
        (result-length ?reslength)
        (test (eq ?reslength (+ 1 ?oplength)))
        (sum ?reschar ?reslength)
        ?fact <- (enum ?reschar ?d)
        (test (neq ?d 1))
        =>
        (retract ?fact)

)

(defrule retract-redundant-result-equal    ; retract leading 0s from result operand
        (declare (salience 100))
        (operand-length ?oplength)
        (result-length ?reslength)
        (test (eq ?reslength ?oplength))
        (sum ?reschar ?reslength)
        ?fact <- (enum ?reschar 0)
        =>
        (retract ?fact)
)


(defrule retract-redundant-operand          ; retract leading 0s from first and second operands
        (declare (salience 100))
        (operand-length ?oplength)
        (first ?op1char ?oplength)
        (second ?op2char ?oplength)
        ?fact1 <- (enum ?op1char 0)
        ?fact2 <- (enum ?op2char 0)
        =>
        (retract ?fact1)
        (retract ?fact2)

)

(defrule retract-redundant-operand-result          ; retract leading 0s from first and second operands
        (declare (salience 100))
        (operand-length ?oplength)
        (result-length ?reslength)
        (test (eq ?reslength (+ 1 ?oplength)))
        (first ?opchar ?oplength)
        (second ?opchar ?oplength)
        ?fact1 <- (enum ?op1char&:(> ?op1char 4))
        ?fact2 <- (enum ?op2char&:(> ?op2char 4))
        =>
        (retract ?fact1)
        (retract ?fact2)

)

(deftemplate previous_column            ; for a column being processed, this carries information of the previous column's
        (multislot letterarray)         ; letter assignments (letterarray) 
        (multislot numberarray)         ; number assignments (numberarray)
        (slot carryover)                ; carryover from previous column
        (slot place)                    ; previous mth column processed
        (slot length)                   ; number of assignments made so far (length of arrays)
)

(deftemplate terminated
        (multislot letterarray)         ; letters assigned
        (multislot numberarray)         ; numbers assigned
        (slot length)                   ; number of assignments (length of arrays)
)

;SPECIAL CASES

(defrule only-one-column-equal        ; handle single columns (e.g A+B=C)

        (operand-length 1)
        (result-length 1)
        
        (first ?f1 1)                   
        (second ?s1 1)
        (sum ?sum1 1)

        (enum ?op1&?f1 ?d1)
        (enum ?op2&?s1 ?d2)
        (enum ?res&?sum1 ?d3)

        (not (assigned ?d1 ?d2 ?d3))
        
        (test (eq (+ ?d1 ?d2) ?d3))

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

        ?current_count  <- (count ?count)

        =>

        (retract ?current_count )
        (assert (count (+ ?count 1)))
        (assert (assigned ?d1 ?d2 ?d3))
        (assert (terminated (letterarray ?f1 ?s1 ?sum1) (numberarray ?d1 ?d2 ?d3) (length 3)))

)

(defrule only-one-column-inequal        ; handle single operands with two digit results (e.g A+B=CD)

        (operand-length 1)
        (result-length 2)
        
        (first ?f1 1)                   
        (second ?s1 1)
        (sum ?sum1 1)
        (sum ?sumn 2)

        (enum ?op1&?f1 ?d1)
        (enum ?op2&?s1 ?d2)
        (enum ?res&?sum1 ?d3)
        (enum ?resn&?sumn 1)

        (not (assigned ?d1 ?d2 1 ?d3))
        
        (test (eq (+ ?d1 ?d2) (+ 10 ?d3)))

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

        (test (if (eq ?f1 ?sumn)
                then
                (eq ?d1 1)
                else
                (neq ?d1 1)))
        
        (test (if (eq ?s1 ?sum1)
                then
                (eq ?d2 ?d3)
                else
                (neq ?d2 ?d3)))

        (test (if (eq ?s1 ?sumn)
                then
                (eq ?d2 1)
                else
                (neq ?d2 1)))

        (test (if (eq ?sum1 ?sumn)
                then
                (eq ?d3 1)
                else
                (neq ?d3 1)))

        ?current_count  <- (count ?count)

        =>

        (retract ?current_count )
        (assert (count (+ ?count 1)))
        (assert (assigned ?d1 ?d2 1 ?d3))
        (assert (terminated (letterarray ?f1 ?s1 ?sumn ?sum1) (numberarray ?d1 ?d2 1 ?d3) (length 4)))

)

; MAIN PROCESSING

(defrule first-column                   ; first column of cryptarithmetic problem
        (declare (salience 10))
        
        (first ?f1 1)                   
        (second ?s1 1)
        (sum ?sum1 1)
        
        (enum ?op1&?f1 ?d1)
        (enum ?op2&?s1 ?d2)
        (enum ?res&?sum1 ?d3)
        
        (test (eq (mod (+ ?d1 ?d2) 10) ?d3))

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

(defrule middle-column                 ; middle column of cryptarithmetic problem
        (declare (salience 20))
        
        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l) )
        
        (first ?fn ?p&~1)
        (second ?sn ?p&~1)
        (sum ?sumn ?p&~1)

        (test (eq ?p (+ ?place 1)))

        (result-length ?result-length&~?p)
        
        (enum ?op1&?fn ?d1)
        (enum ?op2&?sn ?d2)
        (enum ?res&?sumn ?d3)

        (test (eq (mod (+ ?d1 ?d2 ?c) 10) ?d3))   

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
       
        (bind ?c_new (div (+ ?d1 ?d2 ?c) 10))
        (assert (previous_column (letterarray ?fn ?sn ?sumn $?la) (numberarray ?d1 ?d2 ?d3 $?na) (carryover ?c_new) (place (+ ?place 1)) (length (+ 3 ?l))))
)

(defrule result-length-column-equal-length      ; final column if operands and results are equal lengths
        
        (declare (salience 30))

        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l) )

        (operand-length ?length-operand)
        (result-length ?length_res&?length-operand)
        (test (eq ?length-operand (+ ?place 1)))

        (first ?fn ?p&?length_res)
        (second ?sn ?p&?length_res)
        (sum ?sumn ?p&?length_res)

        (enum ?op1&?fn ?d1)
        (enum ?op2&?sn ?d2)
        (enum ?res&?sumn ?d3)

        (test (eq (+ ?d1 ?d2 ?c) ?d3))   
        
        (not (assigned ?d1 ?d2 ?d3 $?na))

        ?current_count <- (count ?count)

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
        (assert (assigned ?d1 ?d2 ?d3 $?na))
        (assert (terminated (letterarray ?fn ?sn ?sumn $?la) (numberarray ?d1 ?d2 ?d3 $?na) (length (+ ?l 3))))

)

(defrule result-length-column-inequal-length    ; final column if operands and results are unequal lengths
        
        (declare (salience 30))

        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l))
    
        (operand-length ?p)
        (result-length ?length_res)
        (test (eq ?length_res (+ ?p 1)))
        (test (eq ?p (+ ?place 1)))

        (first ?fn ?p)
        (second ?sn ?p)
        (sum ?sumn ?p)
        (sum ?sumn_1 ?length_res)

        (test (eq ?length_res (+ ?p 1)))

        (enum ?op1&?fn ?d1)
        (enum ?op2&?sn ?d2)
        (enum ?res&?sumn ?d3)
        (enum ?res_new&?sumn_1 1)

        (not (assigned ?d1 ?d2 ?d3 1 $?na))

        ?current_count <- (count ?count)

        (test (eq (+ ?d1 ?d2 ?c) (+ 10 ?d3))) 

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
                (eq ?d1 1)
                else
                (neq ?d1 1))) 

        (test (if (eq ?sn ?sumn)
                then
                (eq ?d2 ?d3)
                else
                (neq ?d2 ?d3)))

        (test (if (eq ?sn ?sumn_1)
                then
                (eq ?d2 1)
                else
                (neq ?d2 1))) 

        (test (if (eq ?sumn ?sumn_1)
                then
                (eq ?d3 1)
                else
                (neq ?d3 1))) 

        (test (eq (member$ ?fn ?la) (member$ ?d1 ?na)))
        (test (eq (member$ ?sn ?la) (member$ ?d2 ?na)))
        (test (eq (member$ ?sumn ?la) (member$ ?d3 ?na)))
        (test (eq (member$ ?sumn_1 ?la) (member$ 1 ?na)))


        =>     

        (retract ?current_count)
        (assert (count (+ ?count 1)))
        (assert (assigned ?d1 ?d2 ?d3 1 $?na))
        (assert (terminated (letterarray ?fn ?sn ?sumn ?sumn_1 $?la) (numberarray ?d1 ?d2 ?d3 1 $?na) (length (+ ?l 4))))

)


(defrule finish
        (declare (salience 70))

        ?terminated <- (terminated (letterarray $?la) (numberarray $?na) (length ?l))
        (count ?count)
        (not (done ?count))  
        =>
        (printout t " One answer is " crlf)
        (loop-for-count (?cnt 1 ?l) do
                (printout t " Letter " (nth$ ?cnt ?la) " is " (nth$ ?cnt ?na) crlf)
        )
        (printout t " Solution(s): " ?count crlf)
        (printout t crlf)
        (assert (done ?count))
)

(defrule no-solution
        (count 0)
        =>
        (printout t " No solution!" crlf)
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