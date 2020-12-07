program mybase64
    implicit none

    integer, parameter :: BLOCKSIZE = 3072
    integer, parameter :: B64BLOCKSIZE  = BLOCKSIZE / 3 * 4 
    character(*), parameter :: base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

    integer :: argn = 1

    character(len=32) :: optc = ""

    character(len=BLOCKSIZE) :: in = "Man is distinguished"
    character(len=B64BLOCKSIZE) :: out

    call base64_encode(in, 20, out, B64BLOCKSIZE)

    print *, out(1:24)
    ! do
    ! call get_command_argument(argn, optc)
    ! if (len_trim(optc) == 0) exit
    ! argn = argn + 1

    ! end do
    contains
    subroutine base64_encode(inbuf, insize, outbuf, outsize)
        integer, intent(in) :: insize, outsize
        character (*), intent(inout) :: inbuf, outbuf

        integer :: n, n0, n1, n2, n3

        integer :: i, outputIndex=1
        do i = 1, insize, 3
            ! print *, inbuf(i:i+2)
            n = ishft( ichar( in(i:i)), 16 ) 
            n = n + ishft( ichar( in(i+1:i+1)), 8 ) 
            n = n + ichar( in(i+2:i+2))

            
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

    end subroutine base64_encode
    
    ! contains
    ! function do_encode()
    !     character()

    ! end function do_encode
        
end program mybase64