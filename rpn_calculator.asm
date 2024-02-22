SECTION INTVEC

B main

SECTION CODE

;Power function
;parameter:
;   R0: value to pow
;   R1: pow value's
;return:
;   R0 = R0^R1
pow
    PUSH {R1, R2}
    MOV R2, R0
    MOV R0, #1
    pow_loop
        CMP R1, #0
        ASSERT N=0;You can't use a negative exponent
        BLE pow_end_loop
        
        MUL R0, R0, R2
        SUB R1, R1, #1
        B pow_loop
    pow_end_loop
    POP {R1, R2}
    BX LR
    
;Function to right shift a value from a given value
;parameter:
;   R0: value to shift
;   R1: shift number
;return:
;   R0 = R0 >> R1
lsr
    PUSH {R1}
    lsr_loop
        SUBS R1, R1, #1
        LSRPL R0, R0, #1
        BPL lsr_loop
    POP {R1}
    BX LR
    
;Function to left shift a value from a given value
;parameter:
;   R0: value to shift
;   R1: shift number
;return:
;   R0 = R0 << R1
lsl
    PUSH {R1}
    lsl_loop
        SUBS R1, R1, #1
        LSLPL R0, R0, #1
        BPL lsl_loop
    POP {R1}
    BX LR
    
;return: the quotient of the euclidean division
;This function doesn't work with negative number
;parameter:
;   R0: dividend
;   R1: divisor
;return:
;   R0: quotient  
;   R1: remainder  
unsigned_division
    PUSH {R2}
    MOV R2, #0
    CMP R1, #0
    ASSERT Z=0  ;ASSERT: you can't divide by zero
    division_loop
        CMP R0, R1
        BLT division_end_loop
        
        SUB R0, R0, R1
        ADD R2, R2, #1
        B division_loop
    division_end_loop
    MOV R1, R0
    MOV R0, R2
    
    POP {R2}
    BX LR
    
;return: the quotient of the euclidean division
;This function work with negative number
;parameter:
;   R0: dividend
;   R1: divisor
;return:
;   R0: quotient  
;   R1: remainder
division
    PUSH {R2, LR}
    
    CMP R1, #0  ;Test if the divisor is negative
    ASSERT Z=0  ;ASSERT: you can't divide by zero
    BMI negative_divisor
    
    CMP R0, #0  ;Test if the dividend is negative
    BMI negative_dividend
    
    BL unsigned_division;If both are positive we call the unsigned division
    B division_return
    
    negative_divisor
        RSB R1, R1, #0
        BL division
        RSB R0, R0, #0
        B division_return
    
    negative_dividend
        MOV R2, R1  ;Save the divisor value
        RSB R0, R0, #0
        BL division
        
        CMP R1, #0  ;If remainder is null, we only need to negate the quotient
        RSB R0, R0, #0
        BEQ division_return
        
        SUB R0, R0, #1;Else we have to perform these operations
        RSB R1, R1, R2
        
    division_return
    POP {R2, LR}
    BX LR

;Test if the given value in ASCII is a number
;parameter:
;   R0: value
;return:
;   R0: bool
is_ascii_number
    SUBS R0, R0, #0x30
    CMP R0, #9
    MOVGT R0, #0
    BXGT LR
    CMP R0, #0
    MOVGE R0, #1
    MOVLT R0, #0
    BX LR
    
;Test if the given last read ASCII character is a number
;parameter:
;   R0: calcul_stack address
;return:
;   R0: bool
is_number
    PUSH {LR, R1}
    LDR R0, [R0]
    MOV R1, R0
    BL is_ascii_number  ;Test if it's an number represented in ASCII
    CMP R1, #0x2E       ;Or a '.' in ASCII to represent a floating point value
    MOVEQ R0, #1
    POP {LR, R1}
    BX LR

;Test if the last read ASCII characters represent a floating point number
;parameter:
;   R0: number of ASCII character
;   R1: calcul_stack address
;return:
;   R0: bool
is_float
    PUSH {R1-R3, LR}
    MOV R2, R0
    MOV R0, R1
    BL is_number
    CMP R0, #1
    BNE is_float_end
    MOV R0, #0
    is_float_loop
        SUBS R2, R2, #1
        BMI is_float_end
        
        LDR R3, [R1], #4
        CMP R3, #0x2E
        MOVEQ R0, #1
        BNE is_float_loop
    is_float_end
    
    POP {R1-R3, LR}
    BX LR
    
;Convert an integer it's IEE 754 representation
;parameter:
;   R0: value
;return:
;   R0: IEE 754 value
int_to_IEE_754
    PUSH {R1-R3}
    
    ;Get the sign of the integer
    CMP R0, #0
    MOVPL R1, #0
    MOVMI R1, #1
    MVNMI R0, R0        ;If the number is negative we must perform
    ADDMI R0, R0, #1    ;a twos' complement
    
    MOV R2, #127;Initialize the exponent with the bias
    MOV R3, #0;Will store the mantissa
    int_to_IEE_754_shifting_loop
        CMP R2, #0xFF  ;If the number is zero
        MOVEQ R2, #0
        BEQ int_to_IEE_754_shifting_end        
        CMP R0, #1
        BEQ int_to_IEE_754_shifting_end
        
        LSRS R0, R0, #1     ;Shift the mantiss in R3 until we have found the msb
        RRX R3, R3
        
        ADD R2, R2, #1
        
        B int_to_IEE_754_shifting_loop
    int_to_IEE_754_shifting_end
    
    ;Now we only need to extrac the 23 msb of R3
    LSR R3, R3, #9
    
    ORR R0, R3, R2, LSL #23 ;And add the exponent to it
    ORR R0, R0, R1, LSL #31 ;Add the sign
    
    POP {R1-R3}
    BX LR

;FPA less than function
;This function performs return true if the first IEEE 754 value is lower
;than the second IEEE 754 value
;parameter:
;   R0: IEEE 754 value
;   R1: IEEE 754 value
;return:
;   R0: R0 < R1
flt
    PUSH {R1-R4, LR}
    
    ;We first check if operands are not NaN
    PUSH {R0}
    BL is_nan
    ASSERT R0=0;You can't use this function with an operand that is NaN
    MOV R0, R1
    BL is_nan   
    CMP R0, #0
    ASSERT R0=0;You can't use this function with an operand that is NaN
    POP {R0}
    
    ;We then check operands sign
    PUSH {R0}   ;We get the sign of the first operand
    BL get_sign
    MOV R2, R0
    
    
    MOV R0, R1  ;We get the sign of the second operand
    BL get_sign
    MOV R3, R0
    POP {R0}
    
    CMP R2, #0      ;We check if there is sign differences
    CMPEQ R3, #1
    BEQ flt_pos_neg
    CMP R2, #1
    CMPEQ R3, #0
    BEQ flt_neg_pos
    
    ;If they have the same sign we must compare there exponent
    
    PUSH {R0}   ;We get the exponent of the first operand
    BL get_exponent
    MOV R3, R0
    
    MOV R0, R1  ;We get the exponent of the second operand
    BL get_exponent
    MOV R4, R0
    POP {R0}
    
    CMP R2, #0
    BEQ flt_pos_pos
    BNE flt_neg_neg
    
    flt_pos_neg
        MOV R0, #0
        B flt_end
    flt_neg_pos
        MOV R0, #1
        B flt_end
    flt_pos_pos
        CMP R3, R4      ;We compare their exponent
        MOVLT R0, #1
        BLT flt_end
        MOVGT R0, #0
        BGT flt_end
        
        BL flt_mantissa
        
        CMP R3, R4      ;We compare their mantissa
        MOVLT R0, #1
        BLT flt_end
        MOV R0, #0
        B flt_end
        
    flt_neg_neg
        CMP R3, R4      ;We compare their exponent
        MOVLT R0, #0
        BLT flt_end
        MOVGT R0, #1
        BGT flt_end
        
        BL flt_mantissa
        
        CMP R3, R4      ;We compare their mantissa
        MOVGT R0, #1
        BGT flt_end
        MOV R0, #0
        B flt_end
        
    flt_mantissa
        ;If they have the same exponent, then we must compare there mantissas
        PUSH {R0, LR}   ;We get the mantissa of the first operand
        BL get_mantissa
        MOV R3, R0
        
        MOV R0, R1  ;We get the mantissa of the second operand
        BL get_mantissa
        MOV R4, R0
        POP {R0, LR}
        
        BX LR
    
    flt_end
    
    POP {R1-R4, LR}
    BX LR

;FPA greater than function
;This function performs return true if the first IEEE 754 value is greater
;than the second IEEE 754 value
;parameter:
;   R0: IEEE 754 value
;   R1: IEEE 754 value
;return:
;   R0: R0 > R1
fgt
    PUSH {R1-R4, LR}
    
    ;We first check if operands are not NaN
    PUSH {R0}
    BL is_nan
    ASSERT R0=0;You can't use this function with an operand that is NaN
    MOV R0, R1
    BL is_nan   
    CMP R0, #0
    ASSERT R0=0;You can't use this function with an operand that is NaN
    POP {R0}
    
    ;We then check operands sign
    PUSH {R0}   ;We get the sign of the first operand
    BL get_sign
    MOV R2, R0
    
    
    MOV R0, R1  ;We get the sign of the second operand
    BL get_sign
    MOV R3, R0
    POP {R0}
    
    CMP R2, #0      ;We check if there is sign differences
    CMPEQ R3, #1
    BEQ fgt_pos_neg
    CMP R2, #1
    CMPEQ R3, #0
    BEQ fgt_neg_pos
    
    ;If they have the same sign we must compare there exponent
    
    PUSH {R0}   ;We get the exponent of the first operand
    BL get_exponent
    MOV R3, R0
    
    MOV R0, R1  ;We get the exponent of the second operand
    BL get_exponent
    MOV R4, R0
    POP {R0}
    
    CMP R2, #0
    BEQ fgt_pos_pos
    BNE fgt_neg_neg
    
    fgt_pos_neg
        MOV R0, #1
        B fgt_end
    fgt_neg_pos
        MOV R0, #0
        B fgt_end
    fgt_pos_pos
        CMP R3, R4      ;We compare their exponent
        MOVLT R0, #0
        BLT fgt_end
        MOVGT R0, #1
        BGT fgt_end
        
        BL fgt_mantissa
        
        CMP R3, R4      ;We compare their mantissa
        MOVGT R0, #1
        BGT fgt_end
        MOV R0, #0
        B fgt_end
        
    fgt_neg_neg
        CMP R3, R4      ;We compare their exponent
        MOVLT R0, #1
        BLT fgt_end
        MOVGT R0, #0
        BGT fgt_end
        
        BL fgt_mantissa
        
        CMP R3, R4      ;We compare their mantissa
        MOVLT R0, #1
        BLT fgt_end
        MOV R0, #0
        B fgt_end
        
    fgt_mantissa
        ;If they have the same exponent, then we must compare there mantissas
        PUSH {R0, LR}   ;We get the mantissa of the first operand
        BL get_mantissa
        MOV R3, R0
        
        MOV R0, R1  ;We get the mantissa of the second operand
        BL get_mantissa
        MOV R4, R0
        POP {R0, LR}
        
        BX LR
    
    fgt_end
    
    POP {R1-R4, LR}
    BX LR

;Converts read ASCII numbers into value
;parameter:
;   R0: number of ASCII character
;   R1: calcul_stack address
;return:
;   R1: calcul_stack address with new value added in it
convert
    PUSH {R2-R6, R10-R12, LR}
    
    MOV R6, SP  ;We switch to the calcul_stack
    MOV SP, R1
    
    CMP R0, #0
    ASSERT N=0, Z=0;You must pass a number of read ASCII character > 0.
    
    ;Test if the reading character is a float
    PUSH {R0, R1}
    BL is_float
    MOV R10, R0
    POP {R0, R1}
    ;R11 will store a bool to know if we have found the '.', R12
    ;the number of floating point read.
    MOV R11, #0
    MOV R12, #0
    
    MOV R2, #0
    MOV R4, #0
    convert_loop
        CMP R2, R0
        BEQ convert_end_loop
        
        POP {R3}
        
        PUSH {R0}   ;Test if ASCII value is a number or a prefix operator
        MOV R0, R3
        BL is_ascii_number
        CMP R0, #1
        POP {R0}
        
        BEQ convert_number
        
        ;Test to see if we are with a float
        CMP R10, #1
        BNE prefix_operator
        
        CMP R3, #0x2E   ;Check if the character is a '.'
        BNE convert_no_dot
        ASSERT R11=0;You can not have multiple '.' in your declaration.
        MOV R11, #1
        SUB R0, R0, #1
        convert_no_dot
        BEQ convert_loop
        ASSERT R11=1;You can't have operator in the middle of a float declaration
        B prefix_operator
    
        convert_number
            SUB R3, R3, #0x30
            PUSH {R0, R1}   ;Call pow function
            MOV R0, #10
            MOV R1, R2
            BL pow
            MUL R3, R3, R0
            POP {R0, R1}
            
            ADD R4, R4, R3
            
            ADD R2, R2, #1
            
            CMP R10, #1         ;If we are dealing with a float
            CMPEQ  R11, #0      ;and we still not have reach the dot
            ADDEQ R12, R12, #1  ;we increase the floatin point number
            
            B convert_loop
            
        prefix_operator
            SUB R5, R0, R2  ;Calculate the number of remaining ASCII characters
            CMP R5, #1
            BEQ uniq_prefix
            CMP R5, #2
            ASSERT Z=1  ;You can't have perfix with more than two ASCII characters
            B double_prefix
            
            uniq_prefix
                CMP R3, #0x2D
                BEQ negative_prefix
                CMP R3, #0x2B   ;The only perfix with one ASCII character
                ASSERT Z=1      ;allowed are + and -
                positive_prefix
                    B convert_end_loop
                    
                negative_prefix
                    RSB R4, R4, #0
                    B convert_end_loop
                    
            double_prefix
            
                ;If we are with float we must substract with the good power
                CMP R10, #0     ;Else we only need to increment or decrement
                MOVEQ R2, #1    ;by one
                BEQ double_prefix_scale_end
                PUSH {R0, R1}   ;Call pow function
                MOV R0, #10
                SUB R1, R2, #1
                BL pow
                MOV R2, R0
                POP {R0, R1}
                double_prefix_scale_end
                
                CMP R3, #0x2D
                BEQ decrement_prefix
                
                increment_prefix
                    CMP R3, #0x2B
                    ASSERT Z=1
                    POP {R5}        ;The only prefix
                    CMP R5, #0x2B   ;with two ASCII characters
                    ASSERT Z=1      ;allowed are -- and ++
                    
                    ADD R4, R4, R2
                    B convert_end_loop
                decrement_prefix
                    POP {R5}        ;The only prefix
                    CMP R5, #0x2D   ;with two ASCII characters
                    ASSERT Z=1      ;allowed are -- and ++
                    
                    SUB R4, R4, R2
                    B convert_end_loop
    convert_end_loop
    
    
    MOV R1, #0x69   ;ASCII 'i' to mark that this value is a integer
    
    CMP R10, #1             ;If we are with a float
    BNE convert_no_float    ;we convert it into IEE 754 format
    PUSH {R0, R1}
    MOV R0, R4
    MOV R1, R12
    BL convert_to_IEE754
    MOV R4, R0
    POP {R0, R1}
    MOV R1, #0x66   ;Change type to 'f' if the value is a float
    convert_no_float
    
    PUSH {R4}   ;Push the new value in the calcul_stack
    PUSH {R1}   ;Push the type of the value value in the calcul_stack
    
    MOV R1, SP  ;We reswitch to the main stack
    MOV SP, R6
    
    POP {R2-R6, R10-R12, LR}
    BX LR
    
;Compact last read hexadecimal into one value
;You can only compact a maximum of 4 hexadecimal values of 1 byte.
;parameter:
;   R0: number of read hexadecimals
;   R1: calcul_stack address
;return:
;   R1: calcul_stack address with new value added in it
compact
    PUSH {R2-R5, LR}
    
    MOV R5, SP  ;We switch to the calcul_stack
    MOV SP, R1
    
    CMP R0, #0
    ASSERT N=0, Z=0 ;You must pass a number of read hexadecimals
    CMP R0, #5      ;operator between 1
    ASSERT N=1      ;and 4 included
    
    MOV R2, #0
    MOV R4, #0
    compact_loop
        CMP R2, R0
        BEQ compact_end_loop
        
        POP {R3}
        
        PUSH {R0, R1}
        MOV R0, R3
        MOV R1, #8
        MUL R1, R1, R2
        BL lsl
        MOV R3, R0
        POP {R0, R1}
        
        ORR R4, R4, R3
        
        ADD R2, R2, #1
        B compact_loop
    compact_end_loop
    PUSH {R4}   ;Push the new value in the calcul_stack
    
    MOV R1, SP  ;We reswitch to the main stack
    MOV SP, R5
    
    POP {R2-R5, LR}
    BX LR

;Read the transmitted data expression until a separator '[SPACE]' is found
;parameter:
;   R0: data address
;   R1: calcul_stack address
;return:
;   R0: data adresse increased by number of ASCII READ
;   R1: calcul_stack address increased by number of ASCII READ
parse
    PUSH {R2, R3}
    MOV R3, SP
    MOV SP, R1
    loop_parse
        LDRB R2, [R0], #1   ;load ASCII character
        CMP R2, #0x20       ;Test if the character is equal to [SPACE]
        PUSHNE {R2}         ;If not, the carater is push to the stack,
        BNE loop_parse      ;and we continue the loop
    MOV R1, SP
    MOV SP, R3
    POP {R2, R3}
    BX LR

;Function that take a parameter to an RPN calcul and return: it's value
;parameter:
;   R0: data address
;return:
;   R0: result of the RPN calcul
calculator
    PUSH {R1-R3, LR}
    BL init_calcul_stack
    
    calculator_loop
        PUSH {R1}   ;Parse the a list of ASCII characters
        BL parse
        
        ;Calcul the number of ASCII characters read
        MOV R2, R1
        POP {R1}
        PUSH {R2}
        SUB R2, R1, R2
        LSR R2, R2, #2
        POP {R1}
        
        ;Test the last pushed ASCII character to know what to do
        PUSH {R0}
        MOV R0, R1
        BL is_number        ;Test if it's a number or an operator
        CMP R0, #1
        
        BNE calculator_if_nan  ;If it's an operator, we performe the operation
        BEQ calculator_if_number    ;and if it's a number, we convert it
        
        calculator_if_number;Convert the ASCII characters in their number value
            MOV R0, R2
            BL convert
            POP {R0}
            B calculator_loop
        
        calculator_if_nan
            LDR R3, [R1]    ;Test if it's an '#' or an '@' in hexadecimal used
            CMP R3, #0x23   ;to says that hexadecimal value do not represent ASCII
            CMPNE R3, #0x40 ;but integers for '#' and float for '@'
            
            BNE calculator_if_operator
            
            ;If so
            ADD R1, R1, #4  ;We delete the '#' or the '@'
            SUB R0, R2, #1  ;and compact the hexadecimals
            BL compact      ;value on 4 bytes
            CMP R3, #0x23
            MOVEQ R0, #0x69   ;And we add the type of the value
            MOVNE R0, #0x66
            STR R0, [R1, #-4]!
            POP {R0}
            
            B calculator_loop
                
            calculator_if_operator
                MOV R0, R2  ;We first compact the operator on 4 bytes
                BL compact
                
                LDR R0, [R1]
                CMP R0, #0x3D       ;We then test if it is an '='
                BEQ end_calculator  ;to end the calculator
                
                BL calculation      ;Else we perform the calculation
                POP {R0}
                B calculator_loop
    end_calculator
    POP {R0}
    LDR R0, [R1, #8]
    
    POP {R1-R3, LR}
    BX LR
    
;Convert integer and a number of digits  the fractionnal part into
;it's IEEE 754 representation
;parameter:
;   R0: number
;   R1: number of digit in the fractionnal part
;return:
;   R0: IEEE 754 value
convert_to_IEE754
    PUSH {R1-R4, LR}
    
    BL int_to_IEE_754 ;We convert the integer into IEEE 754
    
    LDR R2, =0x41200000;We get 10.0 in IEEE 754
    LDR R3, =0x3F800000;We get 1.0 in IEEE 754
    convert_to_IEE754_loop
        SUBS R1, R1, #1
        BMI convert_to_IEE754_end
        PUSH {R0, R1}   ;Get the power of ten 
        MOV R0, R3      ;that will need to divise
        MOV R1, R2      ;the result by to 
        BL fmul
        MOV R3, R0
        POP {R0, R1}
        B convert_to_IEE754_loop
    convert_to_IEE754_end
    
    ;We then only need to comput R0 / 10^R1
    PUSH {R1}
    MOV R1, R3
    BL fdiv
    POP {R1}
    
    POP {R1-R4, LR}
    BX LR

;Make the calcul and push the result in the calcul_stack
;Note that it is supposed that the top of the pile is an operator
;and that their is at least one numbers under it
;parameter:
;   R1: calcul_stack address
;return:
;   R1: calcul_stack address updated after the calcul
calculation
    PUSH {R0, R2-R8, R10, LR}
    MOV R8, SP
    MOV SP, R1
    POP {R0}
    
    LDR R2, =calcul_stack   ;We calcul the size of the stack
    ADD R2, R2, #400        ;to see if it remain enough numbers
    SUB R2, R2, SP         ;to perform the operations
    LSR R2, R2, #2
    
    MOV R10, #0;Bool activated if we are dealing with floats
    
    ;Switch operator
    
    ;unary operator
    CMP R2, #2  ;Assert remaining numbers >= 1
    ASSERT N=0;There is no remaining numbers to perform the operation
    
    POP {R3, R4}
    ;R3 : type of first operand
    ;R4 : first operand
    
    CMP R0, #0x7E; case ~
    BEQ bitwise_not_op
    CMP R0, #0x21; case !
    BEQ not_op
    LDR R6, =0x2B2B
    CMP R0, R6; case ++
    BEQ increment_op
    LDR R6, =0x2D2D
    CMP R0, R6; case --
    BEQ decrement_op
    LDR R6, =0x616273
    CMP R0, R6; case abs
    BEQ abs_op
    
    ;binary operator
    CMP R2, #4  ;Assert remaining numbers >= 2
    ASSERT N=0;There is not enough remaining numbers to perform the operation
    
    POP {R5, R6}
    ;R5 : type of second operand
    ;R6 : second operand
    
    ;Test if there is float operand
    CMP R3, #0x66
    MOVEQ R10, #1
    CMP R5, #0x66
    MOVEQ R10, #1
    
    CMP R10, #1;Test if we are dealing with floats
    BNE calculation_no_float
    ;If so we perform conversion
    PUSH {R0}
    CMP R3, #0x66;of first operand
    MOVNE R0, R4
    BLNE int_to_IEE_754
    CMP R3, #0x66
    MOVNE R4, R0
    CMP R5, #0x66;of second operand
    MOVNE R0, R6
    BLNE int_to_IEE_754
    CMP R5, #0x66
    MOVNE R6, R0
    POP {R0}
    calculation_no_float
    
    CMP R0, #0x2B; case +
    BEQ addition_op
    CMP R0, #0x2D; case -
    BEQ subtraction_op
    CMP R0, #0x2A; case *
    BEQ multiplication_op
    CMP R0, #0x2F; case /
    BEQ quotient_op
    CMP R0, #0x25; case %
    BEQ remainder_op
    CMP R0, #0x26; case &
    BEQ and_op
    CMP R0, #0x7C; case |
    BEQ or_op
    CMP R0, #0x5E; case ^
    BEQ xor_op
    CMP R0, #0x3E; case >
    BEQ greater_op
    CMP R0, #0x3C; case <
    BEQ less_op
    LDR R7, =0x2A2A
    CMP R0, R7; case **
    BEQ pow_op
    LDR R7, =0x3C3C
    CMP R0, R7; case <<
    BEQ left_shift_op
    LDR R7, =0x3E3E
    CMP R0, R7; case >>
    BEQ right_shift_op
    LDR R7, =0x3E3D
    CMP R0, R7; case >=
    BEQ greater_equal_op
    LDR R7, =0x3C3D
    CMP R0, R7; case <=
    BEQ less_equal_op
    LDR R7, =0x3D3D
    CMP R0, R7; case ==
    BEQ equal_op
    LDR R7, =0x213D
    CMP R0, R7; case !=
    BEQ not_equal_op
    LDR R7, =0x2626
    CMP R0, R7; case &&
    BEQ and_logical_op
    LDR R7, =0x7C7C
    CMP R0, R7; case ||
    BEQ or_logical_op
    
    ASSERT Z=1;The operator is not a valid one
    B break
    
    addition_op
        CMP R10, #0
        BNE addition_op_float
        ADD R0, R6, R4
        B addition_op_end
        addition_op_float
            MOV R0, R6
            MOV R1, R4
            BL fadd
        addition_op_end
        PUSH {R0}
        B break
    subtraction_op
        CMP R10, #0
        BNE subtraction_op_float
        SUB R0, R6, R4
        B subtraction_op_end
        subtraction_op_float
            MOV R0, R6
            MOV R1, R4
            BL fsub
        subtraction_op_end
        PUSH {R0}
        B break
    multiplication_op
        CMP R10, #0
        BNE multiplication_op_float
        MUL R0, R6, R4
        B multiplication_op_end
        multiplication_op_float
            MOV R0, R6
            MOV R1, R4
            BL fmul
        multiplication_op_end
        PUSH {R0}
        B break
    quotient_op
        CMP R10, #0
        BNE division_op_float
        MOV R0, R6
        MOV R1, R4
        BL division
        B quotient_op_end
        division_op_float
            MOV R0, R6
            MOV R1, R4
            BL fdiv
        quotient_op_end
        PUSH {R0}
        B break
    remainder_op
        ASSERT R10=0;You can't use this operator on floats
        MOV R0, R6
        MOV R1, R4
        BL division
        PUSH {R1}
        B break
    pow_op
        ASSERT R10=0;You can't use this operator on floats
        MOV R0, R6
        MOV R1, R4
        BL pow
        PUSH {R0}
        B break
    and_op
        ASSERT R10=0;You can't use this operator on floats
        AND R0, R6, R4
        PUSH {R0}
        B break
    or_op
        ASSERT R10=0;You can't use this operator on floats
        ORR R0, R6, R4
        PUSH {R0}
        B break
    xor_op
        ASSERT R10=0;You can't use this operator on floats
        EOR R0, R6, R4
        PUSH {R0}
        B break
    bitwise_not_op
        CMP R3, #0x66
        ASSERT Z=0;You can't use this operator on a float
        MVN R0, R4
        PUSH {R0}
        B break
    left_shift_op
        ASSERT R10=0;You can't use this operator on floats
        MOV R0, R6
        MOV R1, R4
        BL lsl
        PUSH {R0}
        B break
    right_shift_op
        ASSERT R10=0;You can't use this operator on floats
        MOV R0, R6
        MOV R1, R4
        BL lsr
        PUSH {R0}
        B break
    greater_op
        CMP R10, #0
        BNE greater_op_float
        CMP R6, R4
        MOVGT R0, #1
        MOVLE R0, #0
        B greater_op_end
        greater_op_float
            MOV R0, R6
            MOV R1, R4
            BL fgt
        greater_op_end
        PUSH {R0}
        MOV R10, #0
        B break
    less_op
        CMP R10, #0
        BNE less_op_float
        CMP R6, R4
        MOVLT R0, #1
        MOVGE R0, #0
        B less_op_end
        less_op_float
            MOV R0, R6
            MOV R1, R4
            BL flt
        less_op_end
        PUSH {R0}
        MOV R10, #0
        B break
    greater_equal_op
        CMP R10, #0
        BNE greater_equal_op_float
        CMP R6, R4
        MOVGE R0, #1
        MOVLT R0, #0
        B greater_equal_op_end
        greater_equal_op_float
            MOV R0, R6
            MOV R1, R4
            BL fgt
            CMP R4, R6
            MOVEQ R0, #1
        greater_equal_op_end
        PUSH {R0}
        MOV R10, #0
        B break
    less_equal_op
        CMP R10, #0
        BNE less_equal_op_float
        CMP R6, R4
        MOVLE R0, #1
        MOVGT R0, #0
        B less_equal_op_end
        less_equal_op_float
            MOV R0, R6
            MOV R1, R4
            BL flt
            CMP R4, R6
            MOVEQ R0, #1
        less_equal_op_end
        PUSH {R0}
        MOV R10, #0
        B break
    equal_op
        CMP R4, R6
        MOVEQ R0, #1
        MOVNE R0, #0
        PUSH {R0}
        MOV R10, #0
        B break
    not_equal_op
        CMP R4, R6
        MOVNE R0, #1
        MOVEQ R0, #0
        PUSH {R0}
        MOV R10, #0
        B break
    not_op
        CMP R3, #0x66
        LDREQ R0, =0x7FFFFFFF
        ANDEQ R0, R0, R4
        MOVNE R0, R4
        CMP R0, #0
        MOVEQ R0, #1
        MOVNE R0, #0
        PUSH {R0}
        MOV R10, #0
        B break
    and_logical_op
        CMP R3, #0x66
        LDREQ R0, =0x7FFFFFFF
        ANDEQ R0, R0, R4
        MOVNE R0, R4
        CMP R0, #0
        BEQ and_logical_op_end
        CMP R5, #0x66
        LDREQ R0, =0x7FFFFFFF
        ANDEQ R0, R0, R6
        MOVNE R0, R6
        CMP R0, #0
        MOVNE R0, #1
        and_logical_op_end
        PUSH {R0}
        MOV R10, #0
        B break
    or_logical_op
        CMP R3, #0x66
        LDREQ R0, =0x7FFFFFFF
        ANDEQ R0, R0, R4
        MOVNE R0, R4
        CMP R0, #0
        MOVNE R0, #1
        BNE or_logical_op_end
        CMP R5, #0x66
        LDREQ R0, =0x7FFFFFFF
        ANDEQ R0, R0, R6
        MOVNE R0, R6
        CMP R0, #0
        MOVNE R0, #1
        or_logical_op_end
        PUSH {R0}
        MOV R10, #0
        B break
    increment_op
        CMP R3, #0x66
        BEQ increment_op_float
        ADD R0, R4, #1
        B increment_op_end
        increment_op_float
            MOV R10, #1
            MOV R0, R4
            LDR R1, =0x3F800000;1.0 in IEEE 754
            BL fadd
        increment_op_end
        PUSH {R0}
        B break
    decrement_op
        CMP R3, #0x66
        BEQ decrement_op_float
        SUB R0, R4, #1
        B decrement_op_end
       decrement_op_float
            MOV R10, #1
            MOV R0, R4
            LDR R1, =0x3F800000;1.0 in IEEE 754
            BL fsub
        decrement_op_end
        PUSH {R0}
        B break
    abs_op
        CMP R3, #0x66
        BEQ abs_op_float
        CMP R4, #0
        RSBMI R0, R4, #0
        B abs_op_end
        abs_op_float
            MOV R10, #1
            MOV R0, R4
            BL fabs
        abs_op_end
        PUSH {R0}
        B break
    break
        ;Push on the calcul stack the type of the calculation we have done
        CMP R10, #0
        MOVEQ R0, #0x69
        MOVNE R0, #0x66
        PUSH {R0}
        
        MOV R1, SP  ;Reswitch the stack
        MOV SP, R8
        
        POP {R0, R2-R8, R10, LR}
        BX LR
    
;Return the sign of a a floating-point number represented
;in IEEE 754 format
;parameter:
;   R0: IEEE 754 value
;return:
;   R0: sign of the IEEE 754 value
get_sign
    PUSH {R1}
    LDR R1, =0x80000000;SIGN_MASK
    AND R0, R0, R1
    LSR R0, R0, #31
    POP {R1}
    BX LR
    
;Return the exponent of a a floating-point number represented
;in IEEE 754 format
;parameter:
;   R0: IEEE 754 value
;return:
;   R0: exponent of the IEEE 754 value
get_exponent
    PUSH {R1}
    LDR R1, =0x7F800000;EXPONENT_MASK
    AND R0, R0, R1
    LSR R0, R0, #23
    POP {R1}
    BX LR

;Return the mantissa of a a floating-point number represented
;in IEEE 754 format
;parameter:
;   R0: IEEE 754 value
;return:
;   R0: mantissa of the IEEE 754 value
get_mantissa
    PUSH {R1}
    LDR R1, =0x007FFFFF;MANTISSA_MASK
    AND R0, R0, R1
    POP {R1}
    BX LR

;Test if a floating-point number represented
;in IEEE 754 format is infinity
;parameter:
;   R0: IEEE 754 value
;return:
;   R0 = 0 if not; 1 if positive infinity and 2 if negative infinity
is_infinity
    PUSH {R1, R2}
    LDR R2, =0x7F800000;EXPONENT_MASK
    AND R1, R0, R2
    CMP R1, R2
    MOVNE R0, #0
    BNE end_is_infinity
    MOV R1, #1
    LDR R2, =0x80000000;SIGN_MASK
    ANDS R0, R0, R2
    ADDNE R1, R1, #1
    MOV R0, R1
    end_is_infinity
    POP {R1, R2}
    BX LR
    
;Test if a floating-point number represented
;in IEEE 754 format is NotaNumber
;parameter:
;   R0: IEEE 754 value
;return:
;   R0 = 0 if not, 1 if NaN
is_nan
    PUSH {R1}
    LDR R1, =0xFFFFFFFF;NaN_MASK
    CMP R0, R1
    MOVEQ R0, #1
    MOVNE R0, #0
    POP {R1}
    BX LR

;FPA ADD function
;This function performs addition on floating-point numbers represented
;in IEEE 754 format.
;parameter:
;   R0: first operand
;   R1: second operand
;return:
;   R0 = R0 + R1
fadd
    PUSH {R1-R11, LR}
    
    PUSH {R0}   ;We test if the first operand is
    BL is_nan   ;NotANumber
    CMP R0, #0
    POP {R0}
    BNE fadd_end
    
    PUSH {R0}   ;We test if the first operand is
    MOV R0, R1  ;NotANumber
    BL is_nan   
    CMP R0, #0
    POP {R0}
    MOVNE R0, R1
    BNE fadd_end
    
    ;We test if the first operand is infinity
    PUSH {R0}
    BL is_infinity
    MOV R2, R0
    POP {R0}
    
    ;We test if the second operand is infinity
    PUSH {R0}
    MOV R0, R1
    BL is_infinity
    MOV R3, R0
    POP {R0}
    
    ;We will now handle cases where operands are infinities
    CMP R2, #1
    BEQ fadd_first_operand_inf_pos
    CMP R2, #2
    BEQ fadd_first_operand_inf_neg
    CMP R3, #1
    BEQ fadd_second_operand_inf_pos
    CMP R3, #2
    BEQ fadd_second_operand_inf_neg
    B fadd_no_inf
    fadd_first_operand_inf_pos
        CMP R3, #2              ;If the other operand is negative infinity
        LDREQ R0, =0x7FC00000   ;then the result is undetermined
        B fadd_end              ;else the result is positive infinity
    fadd_first_operand_inf_neg
        CMP R3, #1              ;If the other operand is positive infinity
        LDREQ R0, =0x7FC00000   ;then the result is undetermined
        B fadd_end              ;else the result is negative infinity
    fadd_second_operand_inf_pos
        CMP R2, #2              ;If the other operand is negative infinity
        LDREQ R0, =0x7FC00000   ;then the result is undetermined
        MOVNE R0, R1            ;else the result is positive infinity
        B fadd_end              
    fadd_second_operand_inf_neg
        CMP R2, #1              ;If the other operand is positive infinity
        LDREQ R0, =0x7FC00000   ;then the result is undetermined
        MOVNE R0, R1            ;else the result is negative infinity
        B fadd_end 
    fadd_no_inf
    
    PUSH {R0}   ;We get the exponent of the first operand
    BL get_exponent
    MOV R2, R0
    POP {R0}
    
    PUSH {R0}   ;We get the exponent of the second operand
    MOV R0, R1
    BL get_exponent
    MOV R3, R0
    POP {R0}
    
    CMP R2, R3  
    BCS fadd_no_switch
    MOV R4, R0  ;We switch the first operand with the second operand
    MOV R0, R1  ;if the exponant of the second operand is greater
    MOV R1, R4  ;than the operand of the second operand
    MOV R4, R2
    MOV R2, R3
    MOV R3, R4
    fadd_no_switch
    
    SUB R3, R2, R3  ;We calcul the offset between the exponents
    
    PUSH {R0}   ;We get the mantissa of the first operand
    BL get_mantissa
    MOV R4, R0
    POP {R0}
    
    PUSH {R0}   ;We get the mantissa of the second operand
    MOV R0, R1
    BL get_mantissa
    MOV R5, R0
    POP {R0}
    
    LDR R6, =0x800000
    ORR R4, R4, R6  ;We add the implied bit to the first mantissa
    ORR R5, R5, R6  ;We add the implied bit to the second mantissa
    
    MOV R10, #0
    fadd_shift_mantissa_loop    ;We shift the second mantissa
        SUBS R3, R3, #1         ;by the offset between exponents
        LSRPLS R5, R5, #1       ;while keeping the shifted out bits
        ADCPL R10, R10, R10        ;that will be used later when rounding
        BPL fadd_shift_mantissa_loop
    
    PUSH {R0}   ;We get the sign of the first operand
    BL get_sign
    MOV R6, R0
    POP {R0}
    
    CMP R6, #1  ;We test if the fisrt operand is negative
    BNE fadd_firs_mantissa_negative
    MVN R4, R4  ;If so we perform a two's complement
    ADD R4, R4, #1
    fadd_firs_mantissa_negative
    
    PUSH {R0}   ;We get the sign of the second operand
    MOV R0, R1
    BL get_sign
    MOV R6, R0
    POP {R0}
    
    CMP R6, #1  ;We test if the second operand is negative
    BNE fadd_second_mantissa_negative
    MVN R5, R5  ;If so we perform a two's complement
    ADD R5, R5, #1
    fadd_second_mantissa_negative
    
    ADD R3, R4, R5  ;We add both mantissas
    
    CMP R3, #0          ;If the result is 0
    MOVEQ R2, #0        ;we nullify the exponent
    
    CMP R3, #0     ;We get the sign of the result
    MOVMI R4, #1
    MOVPL R4, #0
    
    CMP R4, #1      ;We test if the result is negative
    BNE fadd_result_mantissa_negative
    MVN R3, R3      ;If so we perform a two's complement
    ADD R3, R3, #1
    fadd_result_mantissa_negative
    
    ADD R2, R2, #9
    ;We now shift the result until we find the msb
    fadd_find_msb_loop
        SUBS R2, R2, #1
        BLE fadd_find_msb_end
        LSRS R10, R10, #1
        ADCS R3, R3, R3          ;Will set the carry on when we will have found the 
        BCC fadd_find_msb_loop  ;msb and delete the implied one at the same time
    fadd_find_msb_end
    
    LDR R6, =0xFFFFFE00 ;The resulting mantissa is the 23 msb bits
    AND R6, R3, R6
    LSR R6, R6, #9
    
    ;To perform a round to nearest, ties to even
    ;We must know if the bit after the last significant bit
    ;is a 0 or a 1 and if there is more 1 after
    AND R5, R3, #0x100   ;Get the bit next to the lsb of the resulting mantissa
    
    ;We also need to know if there is other one
    ;in the bits that was shifted out
    AND R3, R3, #0xFF
    ADD R8, R3, R10
    
    MOV R3, R6
    
    CMP R5, #0          ;We then set a veriable to remeber
    BEQ fadd_lsb_null   ;what rounding operation we should:
    CMP R8, #0          ;0 -> round to lowest
    MOVNE R5, #1        ;1 -> round to upper
    MOVEQ R5, #2        ;2 -> ties to even
    fadd_lsb_null
    
    ;Switch for rounding operations
    CMP R5, #0
    BEQ fadd_end_round
    CMP R5, #1
    BNE fadd_round_tie_to_even
    CMP R4, #1
    BEQ fadd_round_neq
    fadd_round_pos
        ANDS R5, R3, #1
        ADDNE R3, R3, #1
        B fadd_end_round
    fadd_round_neq
        ANDS R5, R3, #1
        ADDEQ R3, R3, #1
        SUBNE R3, R3, #1
        B fadd_end_round
    fadd_round_tie_to_even
        ANDS R5, R3, #1
        ADDNE R3, R3, #1
    fadd_end_round
    
    LSL R4, R4, #31     ;Set the sign at is right position
    LSL R2, R2, #23     ;Set the exponent at is right position
    
    ;We then assemble the result
    ORR R3, R3, R2
    ORR R0, R3, R4
    
    fadd_end
    
    POP {R1-R11, LR}
    BX LR
    
;FPA SUB function
;This function performs subtraction on floating-point numbers represented
;in IEEE 754 format.
;parameter:
;   R0: first operand
;   R1: second operand
;return:
;   R0 = R0 - R1
fsub
    PUSH {R2, LR}
    LDR R2, =0x80000000
    EOR R1, R1, R2
    BL fadd
    POP {R2, LR}
    BX LR
    
;FPA MUL function
;This function performs multiplication on floating-point numbers represented
;in IEEE 754 format.
;parameter:
;   R0: first operand
;   R1: second operand
;return:
;   R0 = R0 * R1
fmul
    PUSH {R1-R11, LR}
    
    PUSH {R0}   ;We test if the first operand is
    BL is_nan   ;NotANumber
    CMP R0, #0
    POP {R0}
    BNE fmul_end
    
    PUSH {R0}   ;We test if the first operand is
    MOV R0, R1  ;NotANumber
    BL is_nan   
    CMP R0, #0
    POP {R0}
    MOVNE R0, R1
    BNE fmul_end
    
    ;We test if the first operand is infinity
    PUSH {R0}
    BL is_infinity
    MOV R2, R0
    POP {R0}
    
    ;We test if the second operand is infinity
    PUSH {R0}
    MOV R0, R1
    BL is_infinity
    MOV R3, R0
    POP {R0}
    
    LDR R4, =0x7FFFFFFF
    ;We will now handle cases where operands are infinities
    CMP R2, #1
    BEQ fmul_first_operand_inf_pos
    CMP R2, #2
    BEQ fmul_first_operand_inf_neg
    CMP R3, #1
    BEQ fmul_second_operand_inf_pos
    CMP R3, #2
    BEQ fmul_second_operand_inf_neg
    B fmul_no_inf
    fmul_first_operand_inf_pos
        TST R1, R4              ;If the other operand is zero
        LDREQ R0, =0xFFFFFFFF   ;then the result is undetermined
        CMP R3, #2              ;Else if the other operand is negative infinity
        MOVEQ R0, R1            ;then the result is negative infinity
        B fmul_end              ;Else the result is positive infinity
    fmul_first_operand_inf_neg
        TST R1, R4              ;If the other operand is zero
        LDREQ R0, =0xFFFFFFFF   ;then the result is undetermined
        CMP R3, #2              ;Else if the other operand is negative infinity
        LDREQ R5, =0x80000000
        EOREQ R0, R0, R5        ;then the result is positive infinity
        B fmul_end              ;Else the result is negative infinity
    fmul_second_operand_inf_pos
        TST R0, R4              ;If the other operand is zero
        LDREQ R0, =0xFFFFFFFF   ;then the result is undetermined
        CMP R2, #2              ;Else if the other operand is not negative infinity
        MOVNE R0, R1            ;then the result is positive infinity
        B fmul_end              ;Else the result is positive infinity
    fmul_second_operand_inf_neg
        TST R0, R4              ;If the other operand is zero
        LDREQ R0, =0xFFFFFFFF   ;then the result is undetermined
        CMP R2, #2              ;Else if the other operand is negative infinity
        LDREQ R5, =0x80000000
        EOREQ R0, R0, R5        ;then the result is positive infinity
        B fmul_end              ;Else the result is negative infinity
    fmul_no_inf
    
    PUSH {R0}   ;We get the sign of the first operand
    BL get_sign
    MOV R2, R0
    POP {R0}
    
    PUSH {R0}   ;We get the sign of the second operand
    MOV R0, R1
    BL get_sign
    MOV R3, R0
    POP {R0}
    
    EOR R2, R2, R3  ;We get the sign of the result
    
    PUSH {R0}   ;We get the exponent of the first operand
    BL get_exponent
    MOV R3, R0
    POP {R0}
    
    PUSH {R0}   ;We get the exponent of the second operand
    MOV R0, R1
    BL get_exponent
    MOV R4, R0
    POP {R0}
    
    ADD R3, R3, R4  ;We calcul the result exponent
    
    CMP R3, #0          ;And handle the case when bot operand
    LSREQ R0, R2, #31   ;are zeros
    BEQ fmul_end
    
    SUB R3, R3, #127
    
    PUSH {R0}   ;We get the mantissa of the first operand
    BL get_mantissa
    MOV R4, R0
    POP {R0}
    
    PUSH {R0}   ;We get the mantissa of the second operand
    MOV R0, R1
    BL get_mantissa
    MOV R5, R0
    POP {R0}
    
    LDR R6, =0x800000
    ORR R4, R4, R6  ;We add the implied bit to the first mantissa
    ORR R5, R5, R6  ;We add the implied bit to the second mantissa
    
    ;Now we perform the multiplication of both mantissas
    ;but we need to perform a 48 bits multiplication
    MOV R6, #0  ;Will store the low bits of the multiplication
    MOV R7, #0  ;Will store the high bits of the multiplication
    MOV R8, #0  ;Will store shifted out bits
    fmul_multiplication_loop
        TST R5, #1
        BNE fmul_multiplication_no_divid
        LSR R5, R5, #1              ;if the mutiplicant is even, we can divide it
        ADDS R4, R4, R4             ;by two and multiple the other by two so that
        ADC R8, R8, R8              ;the multiplication will be faster
        B fmul_multiplication_loop  ;but we also need to remeber the carry
        fmul_multiplication_no_divid
        ADDS R6, R6, R4
        ADC R7, R7, R8
        SUB R5, R5, #1
        CMP R5, #0
        BGT fmul_multiplication_loop
    
    ADD R3, R3, #18
    ;We now shift the 48 bits result until we find the msb
    fmul_find_msb_loop
        SUBS R3, R3, #1
        BLE fmul_find_msb_end
        ADDS R6, R6, R6
        ADCS R7, R7, R7         ;Will set the carry on when we will have found the
        BCC fmul_find_msb_loop  ;msb and delete the implied one at the same time
    fmul_find_msb_end
        
    CMP R3, #0          ;We handle the case when the result
    LSLEQ R0, R2, #31   ;will be zero
    BEQ fmul_end
    
    CMP R3, #128          ;We handle the case when the result
    LDREQ R0, =0x7F800000 ;will be infinite
    EOREQ R0, R0, R2, LSL #31
    BEQ fmul_end
    
    LDR R4, =0xFFFFFE00 ;The resulting mantissa is the 23 msb of
    AND R4, R7, R4      ;the high bits resulting the multiplication
    LSR R4, R4, #9
    
    ;To perform a round to nearest, ties to even
    ;We must know if the bit after the last significant bit
    ;is a 0 or a 1 and if there is more 1 after
    AND R5, R7, #0x100   ;Chech the bit next to the lsb of the resulting mantissa
    
    ;We also need to know if there is other one
    ;in the bits that will be shifted out
    AND R8, R7, #0xFF
    ADD R8, R8, R6
    
    CMP R5, #0     ;We then set a veriable to remeber
    BEQ fmul_lsb_null   ;what rounding operation we should:
    CMP R8, #0          ;0 -> round to lowest
    MOVNE R5, #1        ;1 -> round to upper
    MOVEQ R5, #2        ;2 -> ties to even
    fmul_lsb_null
    
    ;Switch for rounding operations
    CMP R5, #0
    BEQ fmul_end_round
    CMP R5, #1
    BNE fmul_round_tie_to_even
    CMP R2, #1
    BEQ fmul_round_neq
    fmul_round_pos
        ANDS R5, R3, #1
        ADDNE R4, R4, #1
        B fmul_end_round
    fmul_round_neq
        ANDS R5, R3, #1
        ADDEQ R4, R4, #1
        SUBNE R4, R4, #1
        B fmul_end_round
    fmul_round_tie_to_even
        ANDS R5, R3, #1
        ADDNE R4, R4, #1
    fmul_end_round
    
    LSL R2, R2, #31     ;Set the sign at is right position
    LSL R3, R3, #23     ;Set the exponent at is right position
    
    ;We then assemble the result
    ORR R3, R3, R2
    ORR R0, R3, R4
    
    fmul_end
    
    POP {R1-R11, LR}
    BX LR
    
;FPA DIV function
;This function performs division on floating-point numbers represented
;in IEEE 754 format.
;parameter:
;   R0: first operand
;   R1: second operand
;return:
;   R0 = R0 / R1
fdiv
    PUSH {R1-R11, LR}
    
    PUSH {R0}   ;We test if the first operand is
    BL is_nan   ;NotANumber
    CMP R0, #0
    POP {R0}
    BNE fdiv_end
    
    PUSH {R0}   ;We test if the first operand is
    MOV R0, R1  ;NotANumber
    BL is_nan   
    CMP R0, #0
    POP {R0}
    MOVNE R0, R1
    BNE fdiv_end
    
    LDR R4, =0x7FFFFFFF     ;We test if the second operand is
    TST R1, R4              ;zero, if so the result is infinite
    BNE fdiv_no_zero_divisor
    LDR R3, =0x7F800000     ;or undertermined if the first operand is also zero
    EOR R0, R1, R3
    TST R0, R4
    LDREQ R0, =0xFFFFFFFF
    B fdiv_end
    fdiv_no_zero_divisor
    
    ;We test if the first operand is infinity
    PUSH {R0}
    BL is_infinity
    MOV R2, R0
    POP {R0}
    
    ;We test if the second operand is infinity
    PUSH {R0}
    MOV R0, R1
    BL is_infinity
    MOV R3, R0
    POP {R0}
    
    LDR R5, =0x80000000
    ;We will now handle cases where operands are infinities
    CMP R2, #1
    BEQ fdiv_first_operand_inf_pos
    CMP R2, #2
    BEQ fdiv_first_operand_inf_neg
    CMP R3, #1
    BEQ fdiv_second_operand_inf_pos
    CMP R3, #2
    BEQ fdiv_second_operand_inf_neg
    B fdiv_no_inf
    fdiv_first_operand_inf_pos
        TST R1, R4              ;If the other operand is zero
        LDREQ R0, =0xFFFFFFFF   ;then the result is undetermined
        CMP R3, #0              ;Else if the other operand is infinite
        LDRNE R0, =0xFFFFFFFF   ;then the result is also undetermined
        ANDEQ R1, R1, R5        ;Else the result is infinite
        EOREQ R0, R0, R1
        B fmul_end              
    fdiv_first_operand_inf_neg
        TST R1, R4              ;If the other operand is zero
        LDREQ R0, =0xFFFFFFFF   ;then the result is undetermined
        CMP R3, #0              ;Else if the other operand is infinite
        LDRNE R0, =0xFFFFFFFF   ;then the result is also undetermined
        ANDEQ R1, R1, R5        ;Else the result is infinite
        EOREQ R0, R0, R1
        B fmul_end
    fdiv_second_operand_inf_pos
        TST R0, R4              ;If the other operand is zero
        MOVEQ R0, R4            ;then the result is zero
        CMP R3, #0              ;Else if the other operand is infinite
        LDRNE R0, =0xFFFFFFFF   ;then the result is also undetermined
        ANDEQ R0, R0, R5        ;Else the result is zero
        EOREQ R0, R0, R1
        B fmul_end
    fdiv_second_operand_inf_neg
        TST R0, R4              ;If the other operand is zero
        EOREQ R0, R0, R1        ;then the result is zero
        CMP R3, #0              ;Else if the other operand is infinite
        LDRNE R0, =0xFFFFFFFF   ;then the result is also undetermined
        ANDEQ R0, R0, R5        ;Else the result is zero
        EOREQ R0, R0, R1
        B fmul_end
    fdiv_no_inf
    
    PUSH {R0}   ;We get the sign of the first operand
    BL get_sign
    MOV R2, R0
    POP {R0}
    
    PUSH {R0}   ;We get the sign of the second operand
    MOV R0, R1
    BL get_sign
    MOV R3, R0
    POP {R0}
    
    PUSH {R0}   ;We get the exponent of the first operand
    BL get_exponent
    MOV R4, R0
    POP {R0}
    
    PUSH {R0}   ;We get the exponent of the second operand
    MOV R0, R1
    BL get_exponent
    MOV R5, R0
    POP {R0}
    
    PUSH {R0}   ;We get the mantissa of the first operand
    BL get_mantissa
    MOV R6, R0
    POP {R0}
    
    PUSH {R0}   ;We get the mantissa of the second operand
    MOV R0, R1
    BL get_mantissa
    MOV R7, R0
    POP {R0}
    
    ;To perform the division we will use the Newton–Raphson
    ;algorithm in order to calcul 1/denominator and then calculate
    ;numerator * (1/denominator)
    
    ;We must set the numerator and the deniminator between 0.5 and 1.0
    LDR R8, =0x3F000000
    ORR R6, R6, R8
    ORR R7, R7, R8
    
    ;We now need to precomputed values
    LDR R10, =0x4034B4B5    ;48.0 / 17.0
    LDR R11, =0x3FF0F0F1    ;32.0 / 17.0
    
    ;And to calcul x = 48/17 - (32/17 * denominator);
    
    MOV R0, R11
    MOV R1, R7
    BL fmul
    MOV R9, R0
    MOV R0, R10
    MOV R1, R9
    BL fsub
    MOV R9, R0
    
    ;Now we must do 3 times the following operation:
    ; x = x + (x * (1 - (denominator * x)))
    MOV R10, #4
    fdiv_loop
        SUBS R10, R10, #1
        BMI fdiv_loop_end
        ;denominator * x
        MOV R0, R7
        MOV R1, R9
        BL fmul
        MOV R1, R0
        ;1 - (denominator * x)
        LDR R0, =0x3F800000
        BL fsub
        ;x * (1 - (denominator * x)
        MOV R1, R9
        BL fmul
        ;x + (x * (1 - (denominator * x)))
        MOV R1, R9
        BL fadd
        MOV R9, R0
        B fdiv_loop
        
    fdiv_loop_end
    
    ;We get the result sign
    EOR R2, R2, R3
    
    ;We must get the offset between exponents
    SUB R3, R4, R5
        
    ;Now we only need to multiply it by the numberator
    MOV R0, R6
    MOV R1, R9
    BL fmul
    
    ;Set the good exponent
    PUSH {R0}
    BL get_exponent
    MOV R1, R0
    POP {R0}
    
    ADD R1, R1, R3  ;Add the calculated offset
    
    ;Switch the exponent in the result
    LDR R3, =0x807fffff
    AND R0, R0, R3
    ORR R0, R0, R1, LSL #23
    
    ;Set the sign
    ORR R0, R0, R2, LSL #31
    
    fdiv_end
    
    POP {R1-R11, LR}
    BX LR
    
;FPA abs function
;This function performs the absolute value of an IEEE 754 value
;parameter:
;   R0: IEEE 754 value
;return:
;   R0 = |R0|
fabs
    PUSH {R1}
    LDR R1, =0x7FFFFFFF;SIGN_MASK
    AND R0, R0, R1
    POP {R1}
    BX LR

;Initialize the the stack used by functions
;parameter:
;   NONE
;return:
;   NONE
init_stack
    LDR SP, =stack
    ADD SP, SP, #400
    BX LR

;Initialize the the stack used for calcul
;parameter:
;   NONE
;return:
;   R1: stack pointer for the calcul_stack
init_calcul_stack
    LDR R1, =calcul_stack
    ADD R1, R1, #400
    BX LR
    
main
    BL init_stack
    BL unit_test
end
B end

unit_test
    PUSH {LR}
    
    LDR R0, =unit_test_1
    BL calculator   ;13 + 4
    ASSERT R0=17
    
    LDR R0, =unit_test_2
    BL calculator   ;328 + 42 - 2
    ASSERT R0=368
    
    LDR R0, =unit_test_3
    BL calculator   ;(3 + 4) * 2
    ASSERT R0=14
    
    LDR R0, =unit_test_4
    BL calculator   ;(3 + 4) * (5 + 6)
    ASSERT R0=77
    
    LDR R0, =unit_test_5
    BL calculator   ;543 + (-412 +4)
    ASSERT R0=135
    
    LDR R0, =unit_test_6
    BL calculator   ;(3**2)**3
    ASSERT R0=729
    
    LDR R0, =unit_test_7
    BL calculator   ;++(-2)**5
    ASSERT R0=-1
    
    LDR R0, =unit_test_8
    BL calculator   ;(1 << 4) >> 4
    ASSERT R0=1
    
    LDR R0, =unit_test_9
    BL calculator   ;0xFF00 / 0x100
    ASSERT R0=255
    
    LDR R0, =unit_test_10
    BL calculator   ;48.0 / 17 = 0.6666667
    LDR R1, =0x4034B4B5
    CMP R0, R1
    ASSERT Z=1
    
    LDR R0, =unit_test_11
    BL calculator   ;Inf * 0.0 = NaN
    LDR R1, =0xFFFFFFFF
    CMP R0, R1
    ASSERT Z=1
    
    LDR R0, =unit_test_12
    BL calculator   ;(-0.5314684 + -354613.38) / (-1.6843168 * 8.164351) = 25787.564
    LDR R1, =0x46C97721
    CMP R0, R1
    ASSERT Z=1
    
    ;A funny compliacted oned
    LDR R0, =unit_test_13
    BL calculator   ;(((-8/6)**4 * (0.5 + (4 << ++2) & (0.675 == --1.675))) / (15 % 6)) - (--3.345 > (9 | 6)) + 4 * (((-16.495 >= 23.4) == (2 < 5)) + (~0 ^ abs(-3)) / (!(0 && 1) * (0x3578 / (45 >> 3))))
    LDR R1, =0xBFAAAAA8; = -1.333333
    CMP R0, R1
    ASSERT Z=1
    
    POP {LR}
    BX LR

SECTION DATA

;Application stack
stack ALLOC32 100
calcul_stack ALLOC32 100

;Calcul for unit test
unit_test_1 ASSIGN8 0x31, 0x33, 0x20, 0x34, 0x20, 0x2B, 0x20, 0x3D, 0x20
unit_test_2 ASSIGN8 0x33, 0x32, 0x38, 0x20, 0x34, 0x32, 0x20, 0x2B, 0x20, 0x32, 0x20, 0x2D, 0x20, 0x3D, 0x20
unit_test_3 ASSIGN8 0x33, 0x20, 0x34, 0x20, 0x2B, 0x20, 0x32, 0x20, 0x2A, 0x20, 0x3D, 0x20
unit_test_4 ASSIGN8 0x33, 0x20, 0x34, 0x20, 0x2B, 0x20, 0x35, 0x20, 0x36, 0x20, 0x2B, 0x20, 0x2A, 0x20, 0x3D, 0x20
unit_test_5 ASSIGN8 0x35, 0x34, 0x33, 0x20, 0x2D, 0x34, 0x31, 0x32, 0x20, 0x34, 0x20, 0x2B, 0x20, 0x2B, 0x20, 0x3D, 0x20
unit_test_6 ASSIGN8 0x33, 0x20, 0x32, 0x20, 0x2A, 0x2A, 0x20, 0x33, 0x20, 0x2A, 0x2A, 0x20, 0x3D, 0x20
unit_test_7 ASSIGN8 0x2D, 0x32, 0x20, 0x2B, 0x2B, 0x20, 0x35, 0x20, 0x2A, 0x2A, 0x20, 0x3D, 0x20
unit_test_8 ASSIGN8 0x31, 0x20, 0x34, 0x20, 0x3C, 0x3C, 0x20, 0x34, 0x20, 0x3E, 0x3E, 0x20, 0x3D, 0x20
unit_test_9 ASSIGN8 0xFF, 0x00, 0x23, 0x20, 0x1, 0x00, 0x23, 0x20, 0x2F, 0x20, 0x3D, 0x20
unit_test_10 ASSIGN8 0x34, 0x38, 0x2E, 0x20, 0x31, 0x37, 0x20, 0x2F, 0x20, 0x3D, 0x20
unit_test_11 ASSIGN8 0x7F, 0x80, 0x00, 0x00, 0x40, 0x20, 0x30, 0x2E, 0x20, 0x2A, 0x20, 0x3D, 0x20
unit_test_12 ASSIGN8 0x2D, 0x2E, 0x35, 0x33, 0x31, 0x34, 0x36, 0x38, 0x34, 0x20, 0x2D, 0x33, 0x35, 0x34, 0x36, 0x31, 0x33, 0x2E, 0x33, 0x38, 0x20, 0x2B, 0x20, 0x2D, 0x31, 0x2E, 0x36, 0x38, 0x34, 0x33, 0x31, 0x36, 0x38, 0x20, 0x38, 0x2E, 0x31, 0x36, 0x34, 0x33, 0x35, 0x31, 0x20, 0x2A, 0x20, 0x2F, 0x20, 0x3D, 0x20
unit_test_13 ASSIGN8 0x2D, 0x38, 0x20, 0x36, 0x20, 0x2F, 0x20, 0x34, 0x20, 0x2A, 0x2A, 0x20, 0x2E, 0x35, 0x20, 0x34, 0x20, 0x2B, 0x2B, 0x32, 0x20, 0x3C, 0x3C, 0x20, 0x2E, 0x36, 0x37, 0x35, 0x20, 0x2D, 0x2D, 0x31, 0x2E, 0x36, 0x37, 0x35, 0x20, 0x3D, 0x3D, 0x20, 0x26, 0x20, 0x2B, 0x20, 0x2A, 0x20, 0x31, 0x35, 0x20, 0x36, 0x20, 0x25, 0x20, 0x2F, 0x20, 0x2D, 0x2D, 0x33, 0x2E, 0x33, 0x34, 0x35, 0x20, 0x39, 0x20, 0x36, 0x20, 0x7C, 0x20, 0x3E, 0x20, 0x2D, 0x20, 0x34, 0x20, 0x2D, 0x31, 0x36, 0x2E, 0x34, 0x39, 0x35, 0x20, 0x32, 0x33, 0x2E, 0x34, 0x20, 0x3E, 0x3D, 0x20, 0x32, 0x20, 0x35, 0x20, 0x3C, 0x20, 0x3D, 0x3D, 0x20, 0x30, 0x20, 0x7E, 0x20, 0x2D, 0x33, 0x20, 0x61, 0x62, 0x73, 0x20, 0x5E, 0x20, 0x30, 0x20, 0x31, 0x20, 0x26, 0x26, 0x20, 0x21, 0x20, 0x35, 0x78, 0x23, 0x20, 0x34, 0x35, 0x20, 0x33, 0x20, 0x3E, 0x3E, 0x20, 0x2F, 0x20, 0x2A, 0x20, 0x2F, 0x20, 0x2B, 0x20, 0x2A, 0x20, 0x2B, 0x20, 0x3D, 0x20