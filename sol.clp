(deffacts init                          ; initialize digits for letters
        (digit 0 1 2 3 4 5 6 7 8 9) 
)

(defrule input                          ; takes input
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

        (bind ?op1length (length$ (create$ ?op1array)))
        (bind ?op2length (length$ (create$ ?op2array)))
        (bind ?reslength (length$ (create$ ?resarray)))
        (printout t "Length of operands is " ?op1length " & " ?op2length  " & result is " ?reslength crlf)
        
        (assert (operand1-length ?op1length))
        (assert (operand2-length ?op2length))
        (assert (result-length ?reslength))
        
        (loop-for-count (?cnt 1 ?op1length) do
                (bind ?d (+ (- ?op1length ?cnt) 1))
                (bind ?op1char (nth$ ?d ?op1array))
                (assert (first ?op1char ?cnt))

        )

        (loop-for-count (?cnt 1 ?op2length) do
                (bind ?d (+ (- ?op2length ?cnt) 1))
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
        
(defrule enumerate-all-letters          ; enumerate all letters first
        (declare (salience 10))
        (letters $? ?a $?)
        (digit $? ?d $?)
        =>
        (assert (enum ?a ?d))
)

(defrule retract-redundant-res-unequal  ; retract all numbers except 1 from the residual result letter (e.g OH+NO=LOL will have L=1)
        (declare (salience 150))
        (operand1-length ?oplength)
        (operand2-length ?oplength)     
        (result-length ?reslength)
        (test (eq ?reslength (+ 1 ?oplength)))
        (sum ?reschar ?reslength)
        ?fact <- (enum ?reschar ?d&~1)
        ?fact1 <- (enum ?reschar_n&~?reschar 1)
        =>
        (retract ?fact)
        (retract ?fact1)

)

(defrule retract-redundant-operand1         ; retract leading 0s for operand1
        (declare (salience 100))
        (operand1-length ?op1length)
        (first ?op1char ?op1length)
        ?fact1 <- (enum ?op1char 0)
        =>
        (retract ?fact1)

)

(defrule retract-redundant-operand2         ; retract leading 0s for operand2
        (declare (salience 100))
        (operand2-length ?op2length)
        (second ?op2char ?op2length)
        ?fact2 <- (enum ?op2char 0)
        =>
        (retract ?fact2)

)

(defrule retract-redundant-result         ; retract leading 0s for result
        (declare (salience 100))
        (result-length ?reslength) 
        (sum ?reschar ?reslength)
        ?fact <- (enum ?reschar 0)
        =>
        (retract ?fact)

)


(defrule retract-redundant-operand-result-unequal-efficiency          ; retract numbers>5 from first and second operands
        (declare (salience 100))
        (operand1-length ?oplength)
        (operand2-length ?oplength) 
        (result-length ?reslength)
        (test (eq ?reslength (+ 1 ?oplength)))
        (first ?opchar ?oplength)
        (second ?opchar ?oplength)
        ?fact <- (enum ?opchar ?d&:(> 5 ?d))
        =>
        (retract ?fact)

)

(defrule retract-redundant-operand-result-equal-efficiency          ; retract numbers<5 from first and second operands
        (declare (salience 100))
        (operand1-length ?oplength)
        (operand2-length ?oplength) 
        (result-length ?oplength)
        (first ?opchar ?oplength)
        (second ?opchar ?oplength)
        ?fact <- (enum ?opchar ?d&:(> ?d 4))
        =>
        (retract ?fact)

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
        (declare (salience 10))
        (operand1-length 1)
        (operand2-length 1) 
        (result-length 1)
        
        (first ?f1 1)                   
        (second ?s1 1)
        (sum ?sum1 1)

        (enum ?f1 ?d1)
        (enum ?s1 ?d2)
        (enum ?sum1 ?d3)

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

(defrule only-one-column-unequal        ; handle single operands with two digit results (e.g A+B=CD)
        (declare (salience 10))
        (operand1-length 1)
        (operand2-length 1) 
        (result-length 2)
        
        (first ?f1 1)                   
        (second ?s1 1)
        (sum ?sum1 1)
        (sum ?sumn 2)

        (enum ?f1 ?d1)
        (enum ?s1 ?d2)
        (enum ?sum1 ?d3)
        (enum ?sumn 1)

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
        
        (enum ?f1 ?d1)
        (enum ?s1 ?d2)
        (enum ?sum1 ?d3)
        
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
        
        (enum ?fn ?d1)
        (enum ?sn ?d2)
        (enum ?sumn ?d3)

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

(defrule middle-column-first-sum                ; middle column of cryptarithmetic problem when there's no second operand
        (declare (salience 20))
        
        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l) )
        
        (first ?fn ?p&~1)
        (not (second ?sn ?p&~1))
        (sum ?sumn ?p&~1)

        (test (eq ?p (+ ?place 1)))

        (result-length ?result-length&~?p)
        (operand1-length ?length-operand1)
        (operand2-length ?length-operand2&~?length-operand1)
        
        (enum ?fn ?d1)
        (enum ?sumn ?d3)

        (test (eq (mod (+ ?d1 ?c) 10) ?d3))   

        (test (eq (member$ ?fn ?la) (member$ ?d1 ?na)))
        (test (eq (member$ ?sumn ?la) (member$ ?d3 ?na)))    
     

        (test (if (eq ?fn ?sumn)
                then
                (eq ?d1 ?d3)
                else
                (neq ?d1 ?d3)))

        =>     
       
        (bind ?c_new (div (+ ?d1 ?c) 10))
        (assert (previous_column (letterarray ?fn ?sumn $?la) (numberarray ?d1 ?d3 $?na) (carryover ?c_new) (place (+ ?place 1)) (length (+ 2 ?l))))
)

(defrule middle-column-second-sum                ; middle column of cryptarithmetic problem when there's no first operand
        (declare (salience 20))
        
        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l) )
        
        (not (first ?sn ?p&~1))
        (second ?fn ?p&~1)
        (sum ?sumn ?p&~1)

        (test (eq ?p (+ ?place 1)))

        (result-length ?result-length&~?p)
        (operand2-length ?length-operand2)
        (operand1-length ?length-operand1&~?length-operand2)
        
        (enum ?fn ?d1)
        (enum ?sumn ?d3)

        (test (eq (mod (+ ?d1 ?c) 10) ?d3))   

        (test (eq (member$ ?fn ?la) (member$ ?d1 ?na)))
        (test (eq (member$ ?sumn ?la) (member$ ?d3 ?na)))    
     

        (test (if (eq ?fn ?sumn)
                then
                (eq ?d1 ?d3)
                else
                (neq ?d1 ?d3)))

        =>     
       
        (bind ?c_new (div (+ ?d1 ?c) 10))
        (assert (previous_column (letterarray ?fn ?sumn $?la) (numberarray ?d1 ?d3 $?na) (carryover ?c_new) (place (+ ?place 1)) (length (+ 2 ?l))))
)

(defrule result-length-column-equal-length-first-last     ; final column if first operand and result are equal lengths and there's no second operand
        
        (declare (salience 30))

        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l) )

        (operand1-length ?length1-operand)
        (operand2-length ?length2-operand)

        (result-length ?length_res&?length1-operand&~?length2-operand)
        (test (eq ?length1-operand (+ ?place 1)))

        (first ?fn ?p&?length_res)
        (not (second ?sn ?p&?length_res))
        (sum ?sumn ?p&?length_res)

        (enum ?fn ?d1)
        (enum ?sumn ?d3)

        (test (eq (+ ?d1 ?c) ?d3))   
        
        (not (assigned ?d1 ?d3 $?na))

        ?current_count <- (count ?count)


        (test (if (eq ?fn ?sumn)
                then
                (eq ?d1 ?d3)
                else
                (neq ?d1 ?d3)))


        (test (eq (member$ ?fn ?la) (member$ ?d1 ?na)))
        (test (eq (member$ ?sumn ?la) (member$ ?d3 ?na)))

        =>     

        (retract ?current_count)
        (assert (count (+ ?count 1)))
        (assert (assigned ?d1 ?d3 $?na))
        (assert (terminated (letterarray ?fn ?sumn $?la) (numberarray ?d1 ?d3 $?na) (length (+ ?l 2))))

)

(defrule result-length-column-unequal-length-first-last   ; final column if operands and results are unequal lengths and there's no second operand
        
        (declare (salience 30))

        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l))
    
        (operand1-length ?p)
        (operand2-length ?p_new&~?p)
        (result-length ?length_res)
        (test (eq ?length_res (+ ?p 1)))
        (test (eq ?p (+ ?place 1)))

        (first ?fn ?p)
        (not (second ?sn ?p))
        (sum ?sumn ?p)
        (sum ?sumn_1 ?length_res)

        (test (eq ?length_res (+ ?p 1)))

        (enum ?fn ?d1)
        (enum ?sumn ?d3)
        (enum ?sumn_1 1)

        (not (assigned ?d1 ?d3 1 $?na))

        ?current_count <- (count ?count)

        (test (eq (+ ?d1 ?c) (+ 10 ?d3))) 

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

        (test (if (eq ?sumn ?sumn_1)
                then
                (eq ?d3 1)
                else
                (neq ?d3 1))) 

        (test (eq (member$ ?fn ?la) (member$ ?d1 ?na)))
        (test (eq (member$ ?sumn ?la) (member$ ?d3 ?na)))
        (test (eq (member$ ?sumn_1 ?la) (member$ 1 ?na)))


        =>     

        (retract ?current_count)
        (assert (count (+ ?count 1)))
        (assert (assigned ?d1 ?d3 1 $?na))
        (assert (terminated (letterarray ?fn ?sumn ?sumn_1 $?la) (numberarray ?d1 ?d3 1 $?na) (length (+ ?l 3))))

)


(defrule result-length-column-equal-length-second-last     ; final column if second operand and result are equal lengths and there's no first operand
        
        (declare (salience 30))

        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l) )

        (operand2-length ?length2-operand)
        (operand1-length ?length1-operand)

        (result-length ?length_res&?length2-operand&~?length1-operand)
        (test (eq ?length2-operand (+ ?place 1)))

        (not (first ?sn ?p&?length_res))
        (second ?fn ?p&?length_res)
        (sum ?sumn ?p&?length_res)

        (enum ?fn ?d1)
        (enum ?sumn ?d3)

        (test (eq (+ ?d1 ?c) ?d3))   
        
        (not (assigned ?d1 ?d3 $?na))

        ?current_count <- (count ?count)


        (test (if (eq ?fn ?sumn)
                then
                (eq ?d1 ?d3)
                else
                (neq ?d1 ?d3)))


        (test (eq (member$ ?fn ?la) (member$ ?d1 ?na)))
        (test (eq (member$ ?sumn ?la) (member$ ?d3 ?na)))

        =>     

        (retract ?current_count)
        (assert (count (+ ?count 1)))
        (assert (assigned ?d1 ?d3 $?na))
        (assert (terminated (letterarray ?fn ?sumn $?la) (numberarray ?d1 ?d3 $?na) (length (+ ?l 2))))

)

(defrule result-length-column-unequal-length-second-last   ; final column if operands and results are unequal lengths and there's no first operand
        
        (declare (salience 30))

        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l))
    
        (operand2-length ?p)
        (operand1-length ?p_new&~?p)
        (result-length ?length_res)
        (test (eq ?length_res (+ ?p 1)))
        (test (eq ?p (+ ?place 1)))

        (not (first ?sn ?p))
        (second ?fn ?p)
        (sum ?sumn ?p)
        (sum ?sumn_1 ?length_res)

        (test (eq ?length_res (+ ?p 1)))

        (enum ?fn ?d1)
        (enum ?sumn ?d3)
        (enum ?sumn_1 1)

        (not (assigned ?d1 ?d3 1 $?na))

        ?current_count <- (count ?count)

        (test (eq (+ ?d1 ?c) (+ 10 ?d3))) 

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

        (test (if (eq ?sumn ?sumn_1)
                then
                (eq ?d3 1)
                else
                (neq ?d3 1))) 

        (test (eq (member$ ?fn ?la) (member$ ?d1 ?na)))
        (test (eq (member$ ?sumn ?la) (member$ ?d3 ?na)))
        (test (eq (member$ ?sumn_1 ?la) (member$ 1 ?na)))


        =>     

        (retract ?current_count)
        (assert (count (+ ?count 1)))
        (assert (assigned ?d1 ?d3 1 $?na))
        (assert (terminated (letterarray ?fn ?sumn ?sumn_1 $?la) (numberarray ?d1 ?d3 1 $?na) (length (+ ?l 3))))

)



(defrule result-length-column-equal-length      ; final column if operands and results are equal lengths
        
        (declare (salience 30))

        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l) )

        (operand1-length ?length-operand)
        (operand2-length ?length-operand)

        (result-length ?length_res&?length-operand)
        (test (eq ?length-operand (+ ?place 1)))

        (first ?fn ?p&?length_res)
        (second ?sn ?p&?length_res)
        (sum ?sumn ?p&?length_res)

        (enum ?fn ?d1)
        (enum ?sn ?d2)
        (enum ?sumn ?d3)

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

(defrule result-length-column-unequal-length    ; final column if operands and results are unequal lengths
        
        (declare (salience 30))

        ?previous_column <- (previous_column (letterarray $?la) (numberarray $?na) (carryover ?c) (place ?place) (length ?l))
    
        (operand1-length ?p)
        (operand2-length ?p)
        (result-length ?length_res)
        (test (eq ?length_res (+ ?p 1)))
        (test (eq ?p (+ ?place 1)))

        (first ?fn ?p)
        (second ?sn ?p)
        (sum ?sumn ?p)
        (sum ?sumn_1 ?length_res)

        (test (eq ?length_res (+ ?p 1)))

        (enum ?fn ?d1)
        (enum ?sn ?d2)
        (enum ?sumn ?d3)
        (enum ?sumn_1 1)

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


(defrule finish                         ; fires when a solution is found, printing it
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

(defrule no-solution                    ; fires when there's no solution, has the lowest salience so that it is checked at the end
        (count 0)
        =>
        (printout t " No solution!" crlf)
)