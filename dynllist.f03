! Author: Puneet Sandher
! Date: April 5, 2024
! Module that holds subprograms related to the linked lists for the program Unbounded

module dynllist
    type :: node 
          integer :: data 
          type(node), pointer :: next => null()
    end type node
 
    contains 
    
   ! this subroutine inserts a node at the beginning of the linked list
    subroutine insertAtBeginning(head, value)
      type(node), pointer, intent(inout) :: head 

      integer, intent(in) :: value 
      type(node), pointer :: newNode

      allocate(newNode)

      newNode%data = value 
      newNode%next => head
      head =>newNode
   end subroutine insertAtBeginning 

   ! this subroutine inserts a node at the beginning of the linked list
   subroutine insertAtEnd(head, value)
      type(node), pointer, intent(inout) :: head 

      integer, intent(in) :: value 
      type(node), pointer :: newNode, current

      allocate(newNode)

      newNode%data = value 
      newNode%next => null()

      ! Traverse through the linked and store node at the end
      ! if its the beginning of the linked list store as head
      if (.not. associated(head)) then 
         head => newNode
      else 
         current => head 
         
         do while(associated(current%next))
            current => current%next
         end do
         current%next => newNode 
      
      end if
   end subroutine insertAtEnd 

 
    ! Subroutine that clears the linked list
    subroutine clear(head)
       type(node), pointer, intent(inout) :: head
       type(node), pointer ::nextNode
 
       do while (associated(head))
          nextNode => head%next 
          deallocate(nextNode)
          head =>nextNode 
       end do
    end subroutine clear

   !  display is a subroutine that prints the linked list to the terminal
    subroutine display(head)
      type(node), pointer, intent(in) :: head
      type(node), pointer ::nextNode
      nextNode => head 
      do while (associated(nextNode))
         write(*, '(I0, A)', advance='no') nextNode%data
         nextNode => nextNode%next
      end do 
      write(*, *)

    end subroutine display

   !  reverseLinkedList is a subroutine that reverses a linked list
   subroutine reverseLinkedList(head)
      type(node), pointer, intent(inout) :: head
      type(node), pointer :: curNode, prevNode, nextNode 

      prevNode => null()
      curNode => head 

      ! traverse through linked list and swap the nodes
      do while (associated(curNode))
         nextNode => curNode%next 
         curNode%next => prevNode
         prevNode => curNode
         curNode => nextNode
      end do 
      
      head => prevNode 
      
    end subroutine reverseLinkedList
    
    ! Subroutine that checks if the head is empty
    logical function isEmpty(head)
       type(node), pointer, intent(in) :: head
       if (associated(head)) then
          isEmpty = .FALSE.
       else 
          isEmpty = .TRUE.
       end if
    end function isEmpty

   !  getLength is a function that returns the length of a linked list
    integer function getLength(head)
      type(node), pointer, intent(in) :: head
      type(node), pointer :: curNode
      
      getLength = 0
      curNode => head
      
      ! traverse through linked list and increment counter
      do while (associated(curNode))
         curNode => curNode%next
         getLength = getLength + 1
      end do       
    end function getLength

   !  deleteFirstNode is a subroutine that deletes the first node in a linked list
    subroutine deleteFirstNode(head)
      type(node), pointer, intent(inout) :: head
      type(node), pointer :: temp
      if (.not. associated(head)) then
          return
      end if

      temp => head
      head => head%next
      deallocate(temp)
   end subroutine deleteFirstNode

   !  getFirstNode is a function that returns the first node in a linked list
   integer function getFirstNode(head)
      type(node), pointer, intent(in) :: head
      if (associated(head)) then
         getFirstNode = head%data
      else
         getFirstNode = -1
      end if
  end function getFirstNode
 
 end module dynllist
  