program mybase64
    use, intrinsic :: iso_fortran_env, only : stdin=>input_unit

    implicit none

    integer, parameter :: EXIT_FAILURE = 1
    integer, parameter :: EXIT_SUCCESS = 0

    integer, parameter :: BLOCKSIZE = 3072
    integer, parameter :: B64BLOCKSIZE  = BLOCKSIZE / 3 * 4 
    character(*), parameter :: base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

    logical :: decode = .false.
    logical :: ignore_garbage = .false.
    integer :: wrap_column = 76
    character(len=1024) :: infile = ""

    integer :: file_descriptor = 1
    logical :: is_infile_seted = .false.
    logical :: end_line = .false.

    integer :: argn = 1
    integer :: io_stat
    character(len=32) :: optc = ""

    if (mod(BLOCKSIZE, 12) /= 0) then
        ! print *, "Error ... "
        call exit()
    end if

    do
        call get_command_argument(argn, optc)
        if (len_trim(optc) == 0) exit
        argn = argn + 1

        select case (optc)
            case ("--")
                call getarg(argn, optc)
                if (argn < iargc()) then
                    print "(A)", "base64: extra operand ‘" // optc // "’"
                    call usage(EXIT_FAILURE)
                end if

                infile = optc
            case ("--help")
                call usage(EXIT_SUCCESS)
            case ("--version")
                print "(a4)", "8.30"
            case ("-i", "--ignore-garbage")
                ignore_garbage = .true.
            case ("-d", "--decode")
                decode = .true.
            case ("-w")
                call getarg(argn, optc)
                argn = argn + 1
                read (optc, *, IOSTAT=io_stat) wrap_column
                if (io_stat /= 0) then
                    print "(A)", "base64: invalid wrap size: ‘" // trim(optc) // "’"
                    call exit(1)
                end if
            case default
                if (optc(1:7) == "--wrap=") then
                    read (optc(8:), *, IOSTAT=io_stat) wrap_column
                    if (io_stat /= 0) then
                        print "(A)", "base64: invalid wrap size: ‘"//trim(optc(8:))//"’"
                        call exit(1)
                    end if
                else if(optc(1:1) == "-") then
                    if(optc(2:2) == "-") then
                        print "(A)", "base64: unrecognized option '" // trim(optc(3:)) // "'"
                    else
                        print "(A)", "base64: invalid option -- '" // trim(optc(2:)) // "'"
                    end if
                    call usage(EXIT_FAILURE)
                else 
                    if (is_infile_seted) then
                        print "(A)", "base64: extra operand ‘" // infile // "’"
                    else
                        infile = optc
                        is_infile_seted = .true.
                    end if
                end if
        end select
    end do

    if (decode) then
        call do_decode(infile, "stdout", ignore_garbage)
    else
        call do_encode(infile, "stdout", wrap_column)
    end if

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

        if (in == "") then
            file_descriptor = stdin
        else
            open(file_descriptor, file=in, status='old')
        end if

        do while (sum == BLOCKSIZE .and. (.not. is_eof))
            ! print *, "Iniciando bloco"
            sum = 0
            do while (sum < BLOCKSIZE)
                call fgetc(file_descriptor, tmp, io)
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
                end_line = .true.
                call base64_encode(inbuf, sum, outbuf, base64lenght(sum))
                call wrap_write(outbuf, base64lenght(sum), wrap_column, current_column)
            end if
        end do

        if (end_line) then
            print "(A)"
        end if
    end subroutine do_encode

    subroutine do_decode(inbuf, outbuf, ignore_garbage)
        character(*), intent(in) :: inbuf
        character(*), intent(in) :: outbuf
        logical :: ignore_garbage

        ! TODO Decode
    end subroutine

    subroutine wrap_write(outbuf, outsize, wrap_column, current_column)
        character (*), intent(inout) :: outbuf
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

    subroutine usage(status)
        integer, intent(in) :: status

        if (status /= EXIT_SUCCESS) then
            print "(A)",  "Try './mybase64 --help' for more information."
        else 
            print "(A)", "Usage: ./mybase64 [OPTION]... [FILE]"
            print "(A)", "Base64 encode or decode FILE, or standard input, to standard output."
        end if

        call exit(status)
    end subroutine

    function base64lenght(x)
        integer :: x, base64lenght
        base64lenght = CEILING(x / 3.0) * 4
    end function
        
end program mybase64