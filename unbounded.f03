! Author: Puneet Sandher
! Date: April 5, 2024
! Unbounded is a program that calculates addition, subtraction, division, multiplication and factorial for large number using linked lists. 

program unbounded 

    use dynllist
    
    character(len=10) :: operation
    logical :: isValid = .false.
    type(node), pointer :: operandOneLL,operandTwoLL, resultLL  => null()
    character :: signOne = "+", signTwo ="+", sign = "+"
    integer ::largeNum
    
    write (*,*) "Fortran Calculator Application"

    ! Get a valid operation from the user 
    do while (.not. isValid)
        write (*,*) "Enter an operation ( + - * / or !): "
        read (*,'(A1)') operation

        if (operation == "+" .or. operation == "-" .or. operation == "*" .or. operation == "!" .or. operation == "/") then 
            isValid = .true.
        else 
            write (*,*) "Invalid input. Please try again."
        end if
    end do

    ! Get valid operands for the operation
    call getOperand(operandOneLL, signOne)

    if (operation /= "!" ) then 
        call getOperand(operandTwoLL, signTwo)
        ! Calculate which operand is larger 
        largeNum = getAbsLargerNum(operandOneLL, operandTwoLL)
    end if    

    

    ! Call the corresponding subprogams for the desired operation 
    if (operation == '+') then 
        
        ! If only one number is negative, it can be calculated using subtraction
        if ( (signOne == "-" .and. signTwo /= "-") .or.  ( (signOne /= "-" .and. signTwo == "-")) ) then 
            call subtraction(operandOneLL, operandTwoLL, resultLL)
            if(largeNum == 1 ) then 
                sign =  signOne
            elseif (largeNum == 2) then
                sign =  signTwo
            else if (largeNum == 0) then 
                sign = ""
            end if
            
        else 
            ! If both numbers have the same sign, addition is used 
            call addition(operandOneLL, operandTwoLL, resultLL)
            if (signOne == "-" .and. signTwo == "-") then
                sign = "-"
            else 
                sign = "+"
            end if
        end if
        
    else if (operation == '-') then 
        ! Calculate which operand is larger 
        largeNum = getAbsLargerNum(operandOneLL, operandTwoLL)

        ! If one of the operands is negative, the necessary operation is addition
        if ( (signOne == "-" .and. signTwo /= "-") .or.  ( (signOne /= "-" .and. signTwo == "-")) ) then 
            call addition(operandOneLL, operandTwoLL, resultLL)            
        else  
            call subtraction(operandOneLL, operandTwoLL, resultLL)
        end if
        
        ! A result is determined to be positive or negative depending on which operand is larger, and the signs
        if (largeNum == 2 .and. (signOne == '+') .and. (signOne == '+')) then
            sign = "-" 
        else if (largeNum == 1 .and. (signOne == '+') .and. (signOne == '+')) then
            sign = " " 
        else if ( (signOne == "+" .and. signTwo == "-") ) then
            sign = " "
        else if ( (signOne == "-" .and. signTwo == "+") ) then
            sign = "-"
        else if (signOne == "-" .and. signTwo == "-" .and. largeNum == 1 ) then 
            sign = "-"
        else if (signOne == "-" .and. signTwo == "-" .and. largeNum == 2 ) then 
            sign = " "
        else 
            sign = " "
        end if

    else if (operation == '*') then 
        !  get result for multiplication
        call multiplication(operandOneLL, operandTwoLL, resultLL)

        ! If the signs of both operands are the same its positive, else its negative
        if(signOne == "-" .and. signTwo =="-") then 
            sign = " "
        else if (signOne == "+" .and. signTwo =="+") then
            sign = " "
        else 
            sign = "-"
        end if

    else if (operation == '/') then 

        !  get result for division
        call division(operandOneLL, operandTwoLL, resultLL)

        ! If the signs of both operands are the same its positive, else its negative
        if(signOne == "-" .and. signTwo =="-") then 
            sign = " "
        else if (signOne == "+" .and. signTwo =="+") then
            sign = " "
        else 
            sign = "-"
        end if

    else 
        ! get result for factorial
        if(signOne == '-') then 
            write(*,*) "Factorial can be calculated for numbers equal to or greater than 0"
            return
        else 
            call factorial(operandOneLL, resultLL)
            sign = " "
        end if
    end if

    ! Display result
    call displayOutput(resultLL, sign)

    contains    

        ! addition is a subroutine that adds to linked lists and stores it in a result linked list
        subroutine addition(operandOneLL, operandTwoLL, resultLL)

            type(node), pointer, intent(inout) :: operandOneLL, operandTwoLL, resultLL
            type(node), pointer :: num1, num2, iterSum, newNode
            integer :: carryOver, sum

            ! reverse both operand linked list so the least significant digit is first
            call reverseLinkedList(operandOneLL)
            call reverseLinkedList(operandTwoLL)

            num1 => operandOneLL
            num2 => operandTwoLL

            carryOver = 0
            resultLL => null()

            ! While there are still digits to add for either numbers or there is a carry
            do while (associated(num1) .or. associated(num2) .or. carryOver /= 0)
                
                sum = carryOver
                
                ! get the digit at the specific position for both operands and add
                if (associated(num1)) then
                    sum = sum + num1%data
                    num1 => num1%next
                end if
                
                if (associated(num2)) then
                    sum = sum + num2%data
                    num2 => num2%next
                end if
                
                ! If the number is larget than 10, carry it to the left
                carryOver = sum / 10
                sum = mod(sum, 10)
                
                ! create a new node with the sum
                allocate(newNode)
                newNode%data = sum
                newNode%next => null()
                
                ! update results linked list
                if (.not. associated(resultLL)) then
                    resultLL => newNode
                    iterSum => newNode
                else
                    iterSum%next => newNode
                    iterSum => newNode
                end if
            end do

            ! reverse results linked list so the most significant digit is first
            call reverseLinkedList(resultLL)
             
        end subroutine addition

        ! getAbsLargerNum is a function that returns which operand is larger
        integer function getAbsLargerNum(operandOneLL, operandTwoLL)
            type(node), pointer, intent(inout) :: operandOneLL, operandTwoLL
            type(node), pointer :: num1, num2
            integer :: len1, len2

            num1 => operandOneLL
            num2 => operandTwoLL

            len1 = getLength(num1)
            len2 = getLength(num2)

            ! if one operand has a larger length, it has more digits and is larger
            if (len1 > len2) then
                getAbsLargerNum = 1
                return
            elseif (len1 < len2) then
                getAbsLargerNum = 2
                return
            end if

            ! traverse through both linked lists
            ! if the operands have equal lengths, compare each digit until one is larger than the other
            do while (associated(num1) .and. associated(num2))
                if ( (num1%data > num2%data)) then
                    getAbsLargerNum = 1
                    return
                elseif ( (num1%data < num2%data)) then
                    getAbsLargerNum = 2
                    return
                
                end if
                
                num1 => num1%next
                num2 => num2%next
            end do
            
            ! Both linked lists are assumed to be equal if it completes traversing through the linked lists
            getAbsLargerNum = 0

        end function getAbsLargerNum 

        ! subtraction is a subroutine that subtractions two operands and stores the result in a linked list
        subroutine subtraction(operandOneLL, operandTwoLL, resultLL)
            type(node), pointer, intent(inout) :: operandOneLL, operandTwoLL, resultLL
            type(node), pointer :: num1, num2, iterRes, newNode

            integer :: borrow, diff, largerNum

            largerNum = getAbsLargerNum(operandOneLL, operandTwoLL)
            borrow = 0
            resultLL => null()

            ! reverse both operand linked list so the least significant digit is first
            call reverseLinkedList(operandOneLL)
            call reverseLinkedList(operandTwoLL)

            ! The first operand will always be the larger the number, as the positive and negative is already determined 
            if(largerNum == 1 .or. largerNum == 0) then 
                num1 => operandOneLL
                num2 => operandTwoLL
            else 
                num1 => operandTwoLL
                num2 => operandOneLL
            end if

            ! While there are still digits to subtract for either numbers 
            do while (associated(num1))

                ! subtract both operands and store in diff
                diff = num1%data - borrow

                if (associated(num2)) then

                    diff = diff - num2%data
                    num2 => num2%next
                end if

                ! if a diff is less than 0, borrow 
                if (diff < 0) then
                    diff = diff + 10
                    borrow = 1
                else
                    borrow = 0
                end if
                
                ! store result in node 
                allocate(newNode)
                newNode%data = mod(diff, 10)
                newNode%next => null()
                
                ! add to linked list
                if (.not. associated(resultLL)) then
                    resultLL => newNode
                    iterRes => newNode
                else
                    iterRes%next => newNode
                    iterRes => newNode
                end if
    
                num1 => num1%next
            end do

            ! reverse results linked list so the most significant digit is first
            call reverseLinkedList(resultLL)

            ! Remove potential leading 0 from the result
            if ( (getFirstNode(resultLL) == 0) .and. (getLength(resultLL) > 1)) then 
                call deleteFirstNode(resultLL)
            end if

        end subroutine subtraction

        ! multiplication is a subroutine that multiplies two operands and stores it in a linked list
        subroutine multiplication(operandOneLL, operandTwoLL, resultLL)
            type(node), pointer, intent(inout) :: operandOneLL, operandTwoLL, resultLL
            type(node), pointer :: loopRes, num1, num2, sumRes
            integer :: carryOver, product, shift, loop, i
            
            ! reverse both operand linked list so the least significant digit is first
            call reverseLinkedList(operandOneLL)
            call reverseLinkedList(operandTwoLL)

            num1 => operandOneLL
            carryOver = 0
            shift = 0
            loop = 0
            
            ! traverse through every digit in the first operand 
            do while(associated(num1))
                num2 => operandTwoLL
                loopRes => null()
                
                do i = 1, shift 
                    call insertAtEnd(loopRes, 0)
                end do

                ! traverse through every digit in the second opperand and multiply
                ! calculate the potential carryover
                ! store in loopRes linked list
                do while(associated(num2))
                    product = num1%data * num2%data + carryOver 
                    
                    carryOver = product / 10
                    
                    call insertAtEnd(loopRes, mod(product, 10))
                    
                    num2 => num2%next 
                end do
                
                ! store carryover in loopRes
                if (carryOver > 0) then 
                    call insertAtEnd(loopRes, carryOver)
                    carryOver = 0
                end if

                ! resultLL stores the current iteration results
                if (.not. associated(resultLL)) then 
                    resultLL => loopRes
                else 
                    ! if it is multplication of one digit operands, lists may need to be reversed 
                    if (loop == 0) then 
                        call reverseLinkedList(resultLL)
                    end if

                    call reverseLinkedList(loopRes)
                    call addition(resultLL, loopRes, sumRes)
                    call reverseLinkedList(resultLL)
                    call reverseLinkedList(loopRes)

                    resultLL => sumRes
                    loop = loop + 1
                end if

                ! shift the to next digit in the first operand
                shift = shift + 1
                num1 => num1%next

            end do

            ! reverse results linked list so the most significant digit is first
            if (loop == 0) then 
                call reverseLinkedList(resultLL)
            end if

        end subroutine multiplication

        ! division is a subroutine that divides two operands and stores the result in a linked list
        subroutine division(operandOneLL, operandTwoLL, resultLL)
            type(node), pointer, intent(inout) :: operandOneLL, operandTwoLL, resultLL
            type(node), pointer :: num1, num2, subtract_res, addLL, addition_res
            
            num1 =>operandOneLL
            num2 => operandTwoLL
            subtract_res => null()
            addition_res => null()
            
            addLL => null()
            call insertAtEnd(addLL, 1)
            call insertAtEnd(resultLL, 0)

            ! if the operand is 0, it returns
            if (getLength(num2) == 1 .and. num2%data == 0) then 
                return 
            end if 
            
            ! iterate through the error while the dividend is greater than the divsior
            do while (getAbsLargerNum(num1, num2) == 1)

                subtract_res => null()

                ! subtract the dividend by the divisor and increment the result linked list by 1
                call subtraction(num1, num2, subtract_res)
                call addition(resultLL, addLL, addition_res)
                
                resultLL => addition_res
                num1 => subtract_res

                subtract_res => null()
                addition_res => null()

                ! reverse the linked list of the divisor back to its original state
                call reverseLinkedList(num2)                
            end do 

            ! if the dividend and divisor are equal increment the result by 1
            if (getAbsLargerNum(num1, num2) == 0) then 
                addition_res => null()
                call addition(resultLL, addLL, addition_res)
                resultLL => addition_res
            end if

        end subroutine division

        ! factorial is a subroutine that calculates the factorial of a linked list and stores in a result linked list
        subroutine factorial(operandOneLL, resultLL)
            type(node), pointer, intent(inout) :: operandOneLL, resultLL
            type(node), pointer :: num1, multiRes, addLL,  index, multiNum, addNum
            
            index => null()
            addLL => null()

            num1 =>operandOneLL

            call insertAtEnd(index, 1)            
            call insertAtEnd(addLL, 1)
            call insertAtEnd(resultLL, 1)

            ! if the number is 0, return to the main program
            if (getLength(num1) == 1 .and. num1%data <= 0 ) then
                return
            end if

            ! iterate through until the index is equal to the operand
            do while(getAbsLargerNum(num1, index) /= 2)

                multiRes => null() 
                addNum => null()
                multiNum => resultLL
                
                ! multiply the index by the current result
                call multiplication(multiNum, index, multiRes)
                
                resultLL => multiRes

                ! reverse linked lists to bring it back to its original state
                call reverseLinkedList(index)
                call reverseLinkedList(multiNum)
                
                ! increment index
                call addition(index, addLL, addNum)
                index => addNum

            end do

        end subroutine factorial

        ! displayOutput is a subroutine that displays the result and its corresponding sign
        subroutine displayOutput(resultLL, sign)
            type(node), pointer, intent(in) :: resultLL
            character :: sign
            write(*, '(A)', advance='no') "The result is: "
            if (sign == "-") then 
                write(*, '(A)', advance='no') "-"
            end if
            
            call display(resultLL)

        end subroutine displayOutput

        ! getOperand is a subroutine that gets a valid operand from the user 
        subroutine getOperand(operandLL, sign)
    
            type(node), pointer, intent(inout) :: operandLL
            character, intent(inout) :: sign
            logical :: isValid
            character(len=256) :: operandStr 
            integer :: i, charToInt
            isValid = .false.

            operandLL => null()
            do while(.not. isValid)
                operandLL => null()
                write (*,*) "Enter operand:"
                read *, operandStr 

                operandStr = adjustl(operandStr) 
                isValid = .true.
        
                do i = 1, len_trim(operandStr)
                    if (i == 1 .and. operandStr(i:i) == "-") then 
                        sign = operandStr(i:i)
                    else if (operandStr(i:i) >= '0' .and. operandStr(i:i) <= '9') then
                        read(operandStr(i:i), *) charToInt
                        call insertAtEnd(operandLL, charToInt)
                    else
                        write(*,*) "Invalid Input. Please only enter integers"
                        isValid = .false.
                        exit  
                    end if
                end do
        
            end do
        
        end subroutine getOperand
        
        
end program unbounded
