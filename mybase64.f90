program mybase64
    use, intrinsic :: iso_fortran_env, only : stdin=>input_unit

    implicit none

    integer, parameter :: EXIT_FAILURE = 1
    integer, parameter :: EXIT_SUCCESS = 0

    integer, parameter :: BLOCKSIZE = 3072
    integer, parameter :: B64BLOCKSIZE  = BLOCKSIZE / 3 * 4 
    character(*), parameter :: base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

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
                    print "(A)", "base64: extra operand ‘" // trim(optc) // "’"
                    call usage(EXIT_FAILURE)
                else if(optc /= "" .and. infile /= "") then
                    print "(A)", "base64: extra operand ‘" // trim(infile) // "’"
                    call usage(EXIT_FAILURE)
                end if

                if(optc /= "") then
                    infile = optc
                end if
            case ("--help")
                call usage(EXIT_SUCCESS)
            case ("--version")
                call version()
            case ("-i", "--ignore-garbage")
                ignore_garbage = .true.
            case ("-d", "--decode")
                decode = .true.
            case ("-w", "--w")
                call getarg(argn, optc)
                argn = argn + 1
                read (optc, *, IOSTAT=io_stat) wrap_column
                if (io_stat /= 0) then
                    print "(A)", "base64: invalid wrap size: ‘" // trim(optc) // "’"
                    call exit(1)
                end if
            case ("--wrap")
                print "(A)", "base64: option '--wrap' requires an argument"
                call usage(EXIT_FAILURE)
            case default
                if (optc(1:7) == "--wrap=") then
                    read (optc(8:), *, IOSTAT=io_stat) wrap_column
                    if (io_stat /= 0) then
                        print "(A)", "base64: invalid wrap size: ‘"//trim(optc(8:))//"’"
                        call exit(1)
                    end if
                else if (optc(1:2) == "-w") then
                    read (optc(3:), *, IOSTAT=io_stat) wrap_column
                    if (io_stat /= 0) then
                        print "(A)", "base64: invalid wrap size: ‘"//trim(optc(3:))//"’"
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
                        print "(A)", "base64: extra operand ‘" // trim(infile) // "’"
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
    subroutine base64_encode(inbuf, insize, outbuf)
        integer, intent(in) :: insize
        character (*), intent(inout) :: inbuf, outbuf

        integer :: n, n0, n1, n2, n3
        integer :: padCount

        integer :: i, outputIndex

        outputIndex=1
        do i = 1, (insize/3) * 3, 3
            n = ishft( ichar( inbuf(i:i)), 16 ) 
            n = n + ishft( ichar( inbuf(i+1:i+1)), 8 ) 
            n = n + ichar( inbuf(i+2:i+2))
            
            n0 = and(ishft(n, -18) , 63) + 1
            outbuf(outputIndex:outputIndex) = base64chars(n0:n0)

            n1 = and(ishft(n, -12) , 63) + 1
            outbuf(outputIndex+1:outputIndex+1) = base64chars(n1:n1)

            n2 = and(ishft(n, -6) , 63) + 1
            outbuf(outputIndex+2:outputIndex+2) = base64chars(n2:n2)

            n3 = and(n , 63) + 1
            outbuf(outputIndex+3:outputIndex+3) = base64chars(n3:n3)

            outputIndex = outputIndex+4
        end do

        select case(mod(insize, 3))
            case(1)
                n = ishft( ichar( inbuf(i:i)), 16)
                n0 = and(ishft(n, -18) , 63) + 1
                outbuf(outputIndex:outputIndex) = base64chars(n0:n0)

                n1 = and(ishft(n, -12) , 63) + 1
                outbuf(outputIndex+1:outputIndex+1) = base64chars(n1:n1)
                outbuf(outputIndex+2:outputIndex+2) = '='
                outbuf(outputIndex+3:outputIndex+3) = '='
            case(2)
                n = ishft( ichar( inbuf(i:i)), 16 ) 
                n = n + ishft( ichar( inbuf(i+1:i+1)), 8 )

                n0 = and(ishft(n, -18) , 63) + 1
                outbuf(outputIndex:outputIndex) = base64chars(n0:n0)

                n1 = and(ishft(n, -12) , 63) + 1
                outbuf(outputIndex+1:outputIndex+1) = base64chars(n1:n1)

                n2 = and(ishft(n, -6) , 63) + 1
                outbuf(outputIndex+2:outputIndex+2) = base64chars(n2:n2)
                outbuf(outputIndex+3:outputIndex+3) = '='
        end select

    end subroutine base64_encode

    subroutine base64_decode(inbuf, insize, outbuf, npos, rest, ignore_garbage)
        integer, intent(in) :: insize
        character (*), intent(inout) :: inbuf, outbuf
        integer, intent(inout) :: npos, rest
        logical, intent(in) :: ignore_garbage

        integer :: n, n0
        integer :: i, outputIndex

        n = 0
        outputIndex = 1
        do i = 1, insize
            n0 = INDEX(base64chars(:), inbuf(i:i))
            n0 = n0 - 1
            if(n0 == -1) then
                if(ignore_garbage) then
                    cycle
                else if(ichar(inbuf(i:i)) == 10) then
                    cycle
                else
                    call invalid_input()
                end if
            else if(n0 == 64) then
                ! estamos no '='
                select case(npos)
                    case(-1)
                        npos = 0
                        cycle
                    case(2)
                        n0 = and(ishft(n, -16), 255)
                        call fput(achar(n0))

                        npos = -1
                        n=0
                        cycle
                    case(3)
                        n0 = and(ishft(n, -16), 255)
                        call fput(achar(n0))

                        n0 = and(ishft(n, -8), 255)
                        call fput(achar(n0))
                        npos = 0
                        n=0
                        cycle
                    case default
                        call invalid_input()
                end select
            else
                ! caso normal
                select case(npos)
                    case(-1)
                        call invalid_input()
                    case(0)
                        n0 = ishft(n0, 18) 
                    case(1)
                        n0 = ishft(n0, 12) 
                    case(2)
                        n0 = ishft(n0, 6) 
                    ! case(3)
                    !     n0 = ishft(n0, 0)
                end select
                n = n + n0
                npos = npos + 1
            end if

            if(npos == 4) then
                n0 = and(ishft(n, -16), 255)
                call fput(achar(n0))

                n0 = and(ishft(n, -8), 255)
                call fput(achar(n0))

                n0 = and(n, 255)
                call fput(achar(n0))

                npos = 0
                n = 0
            end if
        end do
        rest = n
    end subroutine base64_decode

    subroutine do_encode(in, out, wrap_column)
        character(*), intent(in) :: in, out
        integer, intent(in) :: wrap_column

        integer :: current_column
        character(len=BLOCKSIZE) :: inbuf
        character(len=B64BLOCKSIZE) :: outbuf
        integer :: sum, io, status
        logical :: is_eof

        character :: tmp

        sum=BLOCKSIZE
        current_column = 1
        is_eof = .false.

        if (in == "") then
            file_descriptor = stdin
        else
            open(file_descriptor, file=in, status='old', iostat=status)
            if(status /= 0) then
                print "(A)", "base64: "// trim(in) // ": No such file or directory"
            end if
        end if

        do while (sum == BLOCKSIZE .and. (.not. is_eof))
            ! print *, "Iniciando bloco"
            sum = 0
            do while (sum < BLOCKSIZE)
                call fgetc(file_descriptor, tmp, io)
                if (io < 0) then
                    is_eof = .true.
                    exit
                else if (io > 0) then
                    ! error de leitura (parece que o base64 nao se encomoda)
                end if
                inbuf(sum+1:sum+1) = tmp
                sum = sum+1
            end do


            if(sum > 0) then
                end_line = .true.
                call base64_encode(inbuf, sum, outbuf)
                call wrap_write(outbuf, base64lenght(sum), wrap_column, current_column)
            end if
        end do

        if (end_line) then
            print "(A)"
        end if
    end subroutine do_encode

    subroutine do_decode(in, out, ignore_garbage)
        character(*), intent(in) :: in
        character(*), intent(in) :: out
        logical :: ignore_garbage

        character(len=B64BLOCKSIZE) :: inbuf
        character(len=BLOCKSIZE) :: outbuf
        integer :: sum, io
        logical :: is_eof
        integer :: npos=0

        character :: tmp

        integer :: rest, status
        rest=0

        sum=B64BLOCKSIZE
        is_eof = .false.

        if (in == "") then
            file_descriptor = stdin
        else
            open(file_descriptor, file=in, status='old', iostat=status)
            if(status /= 0) then
                print "(A)", "base64: "// trim(in) // ": No such file or directory"
            end if
        end if

        do while (sum == B64BLOCKSIZE .and. (.not. is_eof))
            sum = 0
            do while (sum < B64BLOCKSIZE)
                call fgetc(file_descriptor, tmp, io)
                if (io < 0) then
                    is_eof = .true.
                    exit
                else if (io > 0) then
                    ! error de leitura (parece que o base64 nao se encomoda)
                end if
                inbuf(sum+1:sum+1) = tmp
                sum = sum+1
            end do

            if(sum > 0) then
                call base64_decode(inbuf, sum, outbuf, npos, rest, ignore_garbage)
            end if
        end do

        if(npos /= 0) then
            if(npos >= 2) then
                call fput(achar(and(ishft(rest, -16), 255)))
            end if
            if(npos >= 3) then
                call fput(achar(and(ishft(rest, -8), 255)))
            end if
            call invalid_input()
        end if
    end subroutine

    subroutine wrap_write(outbuf, outsize, wrap_column, current_column)
        character (*), intent(inout) :: outbuf
        integer, intent(in) :: outsize, wrap_column
        integer, intent(inout) :: current_column
        integer :: i
        do i=1, outsize
            if(current_column == wrap_column+1) then
                print '(A)'
                current_column = 1
            end if
            call fput(outbuf(i:i))
            current_column = current_column + 1
        end do
    end subroutine

    subroutine usage(status)
        integer, intent(in) :: status

        if (status /= EXIT_SUCCESS) then
            print "(A)",  "Try 'base64 --help' for more information."
        else 
            print "(A)", "Usage: base64 [OPTION]... [FILE]"
            print "(A)", "Base64 encode or decode FILE, or standard input, to standard output."
            print "(A)"
            print "(A)", "With no FILE, or when FILE is -, read standard input."
            print "(A)"
            print "(A)", "Mandatory arguments to long options are mandatory for short options too."
            print "(A)", "  -d, --decode          decode data"
            print "(A)", "  -i, --ignore-garbage  when decoding, ignore non-alphabet characters"
            print "(A)", "  -w, --wrap=COLS       wrap encoded lines after COLS character (default 76)."
            print "(A)", "                          Use 0 to disable line wrapping"
            print "(A)"
            print "(A)", "      --help     display this help and exit"
            print "(A)", "      --version  output version information and exit"
            print "(A)"
            print "(A)", "The data are encoded as described for the base64 alphabet in RFC 4648."
            print "(A)", "When decoding, the input may contain newlines in addition to the bytes of"
            print "(A)", "the formal base64 alphabet.  Use --ignore-garbage to attempt to recover"
            print "(A)", "from any other non-alphabet bytes in the encoded stream."
            print "(A)"
            print "(A)", "GNU coreutils online help: <https://www.gnu.org/software/coreutils/>"
            print "(A)", "Full documentation at: <https://www.gnu.org/software/coreutils/base64>"
            print "(A)", "or available locally via: info '(coreutils) base64 invocation'"
        end if

        call exit(status)
    end subroutine

    subroutine invalid_input()
        print "(A)", "base64: invalid input"
        call exit(1)
    end subroutine invalid_input

    subroutine version()
        print "(A)", "base64 (GNU coreutils) 8.30"
        print "(A)", "Copyright (C) 2018 Free Software Foundation, Inc."
        print "(A)", "License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>."
        print "(A)", "This is free software: you are free to change and redistribute it."
        print "(A)", "There is NO WARRANTY, to the extent permitted by law."
        print "(A)"
        print "(A)", "Written by Simon Josefsson."
        call exit(0)
    end subroutine version

    function base64lenght(x)
        integer :: x, base64lenght
        base64lenght = CEILING(x / 3.0) * 4
    end function

        
end program mybase64