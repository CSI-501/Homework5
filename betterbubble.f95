program betterbubble
    ! Nicholas Maynard
    ! CSI 501
    ! Homework 5
    ! 03/02/2023
    ! This program implements a better version of the bubble sort
    ! algorithm.
 
    ! Clear memory
    implicit none

    ! Intialize Variables
    character*50 :: InFile
    real, allocatable :: A(:)
    integer, allocatable :: Ind(:)
    integer :: i, j, n, Temp
    logical :: Done

    ! Ask user for File input
    print*, 'Enter input file: '
    read(*,*) InFile
    open(42,file=InFile)
    open(13,file=('Output.txt'))

    ! Skip a header line
    read(42,*)
    ! Read in size of file
    read(42,*) n 
    ! Allocate size of array based on file
    allocate(A(n))
    allocate(Ind(n))

    ! Import Array
    do i = 1, n
        read(42,*) A(i)
        Ind(i) = i
    enddo

    ! Run Better Bubble Sort algorithm with index sort.
    do i = 1, n
        Done = .true.
        do j = 1, n - i
            if (A(Ind(j)) .gt. A(Ind(j+1))) then
                Temp = Ind(j)
                Ind(j) = Ind(j+1)
                Ind(j+1) = Temp
                Done = .false.
            endif
        enddo
        if (Done) exit
    enddo

    ! Write header of the file for test suite to work
    write(13,*) 'Sorted Data'
    write(13,*) n

    ! Output Results
    do i = 1, n
        write(13,*) A(Ind(i))
    enddo 

    ! Write footer of the file for test suite to work
    write(13,*) 'Done!'

    ! Deallocate Memory
    deallocate(A)
    deallocate(Ind)
       
 end program betterbubble