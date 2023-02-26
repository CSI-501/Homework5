program betterbubble
    ! Nicholas Maynard
    ! CSI 501
    ! Homework 5
    ! 03/02/2023
    ! This program implements a better version of the bubble sort
    ! algorithm.
 
    implicit none

    character*50 :: InFile
    real, allocatable :: A(:)
    real :: Temp
    integer :: i, j, n
    logical :: Done

    ! Ask user for File input
    print*, 'Enter input file: '
    read(*,*) InFile
    open(42,file=InFile)
    open(13,file=('sort' // trim(InFile)))

    !Skip a header line
    read(42,*)
    ! Read in size of file
    read(42,*) n 
    ! Allocate size of array based on file
    allocate(A(n))
    ! Import Array
    do i = 1, n
        read(42,*) A(i)
    enddo

    ! Run Better Bubble Sort algorithm.
    do i = 1, n
        Done = .true.
        do j = 1, n - i
            if (A(j) .gt. A(j+1)) then
                Temp = A(j)
                A(j) = A(j+1)
                A(j+1) = Temp
                Done = .false.
            endif
        enddo
        if (Done) exit
    enddo

    ! Output Results
    do i = 1, n
        write(13,*) A(i)
    enddo 

    ! Deallocate Memory
    deallocate(A)
       
 end program betterbubble