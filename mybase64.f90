program mybase64
    ! use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, &
    !                                       stdout=>output_unit, &
    !                                       stderr=>error_unit

    implicit none

    integer, parameter :: BLOCKSIZE = 3072
    integer, parameter :: B64BLOCKSIZE  = BLOCKSIZE / 3 * 4 
    character(*), parameter :: base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

    logical :: decode = .false.
    logical :: ignore_garbage = .false.
    integer :: wrap_column = 76
    character(len=1024) :: infile = "test.txt"

    integer :: argn = 1
    character(len=32) :: optc = ""

    if (mod(BLOCKSIZE, 12) /= 0) then
        ! print *, "Error ... "
        call exit()
    end if


    ! character(len=BLOCKSIZE) :: in = "Man is distinguished"
    ! character(len=B64BLOCKSIZE) :: out

    ! call base64_encode(in, 20, out, B64BLOCKSIZE)

    ! print *, out(1:24)

    ! do
    ! call get_command_argument(argn, optc)
    ! if (len_trim(optc) == 0) exit
    ! argn = argn + 1

    ! end do

    ! if (decode) then
    !     do_decode()
    ! else
        call do_encode(infile, "stdout", wrap_column)
    ! end if

    contains
    subroutine base64_encode(inbuf, insize, outbuf, outsize)
        integer, intent(in) :: insize, outsize
        character (*), intent(inout) :: inbuf, outbuf

        integer :: n, n0, n1, n2, n3
        integer :: padCount

        integer :: i, outputIndex

        outputIndex=1
        do i = 1, insize, 3
            n = ishft( ichar( inbuf(i:i)), 16 ) 
            n = n + ishft( ichar( inbuf(i+1:i+1)), 8 ) 
            n = n + ichar( inbuf(i+2:i+2))
            
            n0 = and(ishft(n, -18) , 63) + 1
            outbuf(outputIndex:outputIndex) = base64chars(n0:n0)
            ! print *, outbuf(outputIndex:outputIndex)

            n1 = and(ishft(n, -12) , 63) + 1
            outbuf(outputIndex+1:outputIndex+1) = base64chars(n1:n1)
            ! print *, outbuf(outputIndex+1:outputIndex+1)

            n2 = and(ishft(n, -6) , 63) + 1
            outbuf(outputIndex+2:outputIndex+2) = base64chars(n2:n2)
            ! print *, outbuf(outputIndex+2:outputIndex+2)

            n3 = and(n , 63) + 1
            outbuf(outputIndex+3:outputIndex+3) = base64chars(n3:n3)
            ! print *, outbuf(outputIndex+3:outputIndex+3)

            outputIndex = outputIndex+4
        end do

        padCount = mod(insize, 3)
        if (padCount > 0) then
            do i = 3-padCount, 1, -1
                outbuf(outputIndex-i:outputIndex-i) = '='
            end do
        end if

    end subroutine base64_encode

    subroutine do_encode(in, out, wrap_column)
        character(*), intent(in) :: in, out
        integer, intent(in) :: wrap_column

        integer :: current_column
        character(len=BLOCKSIZE) :: inbuf
        character(len=B64BLOCKSIZE) :: outbuf
        integer :: sum, io
        logical :: is_eof

        character :: tmp

        sum=BLOCKSIZE
        current_column = 1
        is_eof = .false.

        open(1, file=in, status='old')

        do while (sum == BLOCKSIZE .and. (.not. is_eof))
            ! print *, "Iniciando bloco"
            sum = 0
            do while (sum < BLOCKSIZE)
                call fgetc(1, tmp, io)
                ! print *, tmp, io
                if (io < 0) then
                    is_eof = .true.
                    exit
                else if (io > 0) then
                    ! error de leitura (parece que o base64 nao se encomoda)
                end if
                inbuf(sum+1:sum+1) = tmp
                sum = sum+1
            end do

            ! print *, inbuf(1:sum)

            if(sum > 0) then
                call base64_encode(inbuf, sum, outbuf, base64lenght(sum))
                call wrap_write(outbuf, base64lenght(sum), wrap_column, current_column)
            end if
        end do

    end subroutine do_encode

    subroutine wrap_write(outbuf, outsize, wrap_column, current_column)
        character (*), intent(in) :: outbuf
        integer, intent(in) :: outsize, wrap_column
        integer, intent(inout) :: current_column
        integer :: i
        do i=1, outsize
            if(current_column == wrap_column+1) then
                ! call fput('\n')
                print '(A)'
                current_column = 1
            end if
            ! write(*,*) outbuf(i:i)
            call fput(outbuf(i:i))
            current_column = current_column + 1
        end do
    end subroutine

    function base64lenght(x)
        integer :: x, base64lenght
        base64lenght = CEILING(x / 3.0) * 4
    end function
        
end program mybase64