! All rights reserved.
! 
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are
! met:
! 
!  Redistributions of source code must retain the above copyright
! notice, this list of conditions and the following disclaimer.
!   Redistributions in binary form must reproduce the above
! copyright notice, this list of conditions and the following disclaimer
! in the documentation and/or other materials provided with the
! distribution.
!   Neither the name of original developer, nor the names of its
! contributors may be used to endorse or promote products derived from
! this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
! LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
! DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
! THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
! OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
module applibrary
    implicit none; private

    public :: split_commandline_into_args

    public :: trim, &
              index

    interface
        subroutine exit_x(context)
            class(*), intent(inout) :: context
        end subroutine
    end interface

    type, public :: argument
        private
        character(:), allocatable, public :: chars
    contains
        procedure, private, pass(lhs) :: argument_assign_argument
        procedure, private, pass(lhs) :: argument_assign_character
        procedure, private, pass(rhs) :: character_assign_argument
        generic :: assignment(=)      => argument_assign_argument,      &  
                                         argument_assign_character,     &
                                         character_assign_argument
        procedure, private, pass(dtv) :: argument_read_formatted
        procedure, private, pass(dtv) :: argument_write_formatted
        procedure, private, pass(dtv) :: argument_read_unformatted !! Unformatted input.
        procedure, private, pass(dtv) :: argument_write_unformatted
        generic :: read (formatted)   => argument_read_formatted
        generic :: write (formatted)  => argument_write_formatted
        generic :: read (unformatted) => argument_read_unformatted
        generic :: write (unformatted)=> argument_write_unformatted
    end type
    
    type, public :: application
        private
        character(:), allocatable           :: username_
        character(:), allocatable           :: location_
        class(*), pointer                   :: context_ => null()
        procedure(exit_x), nopass, pointer  :: onexit_ => null()
        contains
        procedure, pass(this), public :: username => app_username
        procedure, pass(this), public :: location => app_location
        procedure, pass(this), public :: abort => app_abort
        procedure, pass(this), public :: shutdown => app_shutdown
        procedure, pass(this), public :: onexit => app_onexit
        final :: app_finalize
    end type

    interface split_commandline_into_args
        module procedure split_commandline_into_args0
        module procedure split_commandline_into_args1
        module procedure split_commandline_into_args2
        module procedure split_commandline_into_args3
    end interface

    interface trim
        module procedure :: argument_trim
    end interface
    
    interface index
        module procedure :: argument_index
    end interface

    contains

    function app_username(this) result(username)
        class(application), intent(inout) :: this
        !private
        character(:), allocatable :: username

        if (.not. allocated(this%username_)) then
            this%username_ = get_username()
        end if
        username = this%username_
    end function

    function app_location(this) result(location)
        class(application), intent(inout) :: this
        !private
        character(:), allocatable :: location
        integer :: n

        if (.not. allocated(this%location_)) then
            call get_command_argument(0,length=n)
            allocate(character(n) :: this%location_)
            call get_command_argument(0,value=this%location_)
        end if
        location = this%location_
    end function

    subroutine app_abort(this)
        class(application), intent(inout) :: this
#ifndef __GFORTRAN__
        interface
            subroutine abort() bind(C, name="abort")
            end subroutine
        end interface
#endif

        call app_finalize(this)
        call abort()
    end subroutine

    subroutine app_shutdown(this, code)
        class(application), intent(inout) :: this
        integer, intent(in), optional :: code

        call app_finalize(this)

        if (present(code)) then
            error stop code
        else
            stop 0
        end if
    end subroutine

    subroutine app_onexit(this, context, event)
        class(application), intent(inout) :: this
        class(*), intent(in), target :: context
        procedure(exit_x) :: event

        nullify(this%context_); this%context_ => context
        nullify(this%onexit_); this%onexit_ => event
    end subroutine
    
    subroutine app_finalize(this)
        type(application), intent(inout) :: this
        
        if (associated(this%onexit_)) then 
            call this%onexit_(this%context_)
            nullify(this%context_)
            nullify(this%onexit_)
        end if
    end subroutine

    function split_commandline_into_args0() result(args)
        !private
        character(:), allocatable   :: commandline
        type(argument), allocatable :: args(:)
        !private
        integer :: length

        call get_command(length = length)
#ifdef _WIN32
        if (length > 8191) then 
            write(*,*) '<!> The length of the command line exceeds the maximum length allowed on Windows system.'
        end if
#endif
        allocate(character(length) :: commandline)
        call get_command(command = commandline)
        args = split_commandline_into_args1(commandline)
    end function

    function split_commandline_into_args1(commandline) result(args)
        character(*), intent(in)    :: commandline
        type(argument), allocatable :: args(:)

        args = split_commandline_into_args2(commandline, .true.)
    end function

    function split_commandline_into_args2(commandline, removeHashComments) result(args)
        character(*), intent(in)    :: commandline
        logical, intent(in)         :: removeHashComments
        type(argument), allocatable :: args(:)
        !private
        character(1), allocatable   :: dummy(:)

        args = split_commandline_into_args3(commandline, .true., dummy)
    end function

    !> @brief The following command line parsing algorithm is similar to the one
    !! used by Microsoft. The rules used by Microsoft C/C++ code are 
    !! Microsoft-specific. The runtime startup code uses these rules when 
    !! interpreting arguments given on the operating system command line:
    !!  - Arguments are delimited by white space, which is either a space or a tab.
    !!  - The first argument (args[0]) is treated specially. It represents the program name. 
    !!    Because it must be a valid pathname, parts surrounded by double quote marks (") 
    !!    are allowed. The double quote marks aren't included in the args[0] output. 
    !!    The parts surrounded by double quote marks prevent interpretation of a space or 
    !!    tab character as the end of the argument. The later rules in this list don't apply.
    !!  - A string surrounded by double quote marks is interpreted as a single argument, 
    !!    which may contain white-space characters. A quoted string can be embedded in 
    !!    an argument. The caret (^) isn't recognized as an escape character or delimiter. 
    !!    Within a quoted string, a pair of double quote marks is interpreted as a single 
    !!    escaped double quote mark. If the command line ends before a closing double quote 
    !!    mark is found, then all the characters read so far are output as the last argument.
    !!  - A double quote mark preceded by a backslash (\") is interpreted as a literal 
    !!    double quote mark (").
    !!  - Backslashes are interpreted literally, unless they immediately precede a double quote mark.
    !!  - If an even number of backslashes is followed by a double quote mark, then one backslash (\) 
    !!    is placed in the argv array for every pair of backslashes (\\), and the double quote mark 
    !!    (") is interpreted as a string delimiter.
    !!  - If an odd number of backslashes is followed by a double quote mark, then one backslash (\) 
    !!    is placed in the argv array for every pair of backslashes (\\). The double quote mark is 
    !!    interpreted as an escape sequence by the remaining backslash, causing a literal double 
    !!    quote mark (") to be placed in argv.
    !! @note The original code (in C#) can be found here: https://github.com/dotnet/roslyn/blob/46c8f4f56765e16b92759de8098494b36cfb84f5/src/Compilers/Core/Portable/InternalUtilities/CommandLineUtilities.cs
    function split_commandline_into_args3(commandline, removeHashComments, illegalChar) result(args)
        character(*), intent(in)                :: commandline
        logical, intent(in)                     :: removeHashComments
        character(1), allocatable, intent(out)  :: illegalChar(:)
        type(argument), allocatable             :: args(:)
        !private
        character(:), allocatable :: builder
        integer :: i, j, k, n, quoteCount, slashCount
        character(1) :: current
        character(1), parameter :: SPACE = char(32)
        character(1), parameter :: BACKSLASH = char(92)
        character(1), parameter :: QUOTE = char(34)
        character(1), parameter :: HASH = char(35)
        character(1), parameter :: PIPE = char(124)

        n = len(commandline)
        i = 1; j = 1; quoteCount = 0; slashCount = 0
        if (allocated(illegalChar)) deallocate(illegalChar)
        allocate(args(0))

        do while (i <= n)
            do while (i <= n .and. commandline(i:i) == SPACE)
                i = i + 1; if (i > n) exit
            end do

            if (i > n) exit
            if (commandline(i:i) == HASH .and. removeHashComments) exit

            quoteCount = 0
            j = 1
            if (allocated(builder)) deallocate(builder)
            allocate(character(len(commandline)) :: builder)
            do k = 1, n
                builder(k:k) = SPACE
            end do

            do while (i <= n .and. (commandline(i:i) /= SPACE .or. mod(quoteCount, 2) /= 0))
                current = commandline(i:i)
                select_loop: select case (current)
                case (BACKSLASH)
                    slashCount = 0
                    do while (i <= n .and. commandline(i:i) == BACKSLASH)
                        builder(j:j) = commandline(i:i); j = j + 1
                        i = i + 1; if (i > n) exit
                        slashCount = slashCount + 1
                    end do

                    if (i >= n .or. commandLine(i:i) /= QUOTE) exit select_loop

                    if (mod(slashCount, 2) == 0) then
                        quoteCount = quoteCount + 1
                    end if

                    builder(j:j) = QUOTE; j = j + 1
                    i = i + 1; if (i > n) exit
                    exit select_loop
                case (QUOTE)
                    builder(j:j) = current; j = j + 1
                    quoteCount = quoteCount + 1
                    i = i + 1; if (i > n) exit
                    exit select_loop
                case default
                    if ((iachar(current) >= 1 .and. iachar(current) <= 31) .or. current == PIPE) then
                        if (.not. allocated(illegalChar)) illegalChar = current
                    else
                        builder(j:j) = current; j = j + 1
                    end if
                    i = i + 1; if (i > n) exit
                    exit select_loop
                end select select_loop
            end do

            if (quoteCount == 2 .and. builder(1:1) == QUOTE .and. builder(n:n) == QUOTE) then
                builder(1:1) = SPACE
                builder(n:n) = SPACE
            end if

            builder = trim(adjustl(builder(1:n)))
            if (len(builder) > 0) then
                args = [args, argument(builder)]
            end if
        end do
    end function

    function get_username() result(username)
        character(255) :: tmp
        character(:), allocatable :: username
        integer :: sts
#ifdef _WIN32
        call get_environment_variable(name="USERNAME", value=tmp, status=sts)
#else
        call get_environment_variable(name="USER", value=tmp, status=sts)
#endif
        if (sts == 0) then
            username = trim(tmp)
        end if
    end function

    pure subroutine argument_assign_argument(lhs, rhs)
        class(argument), intent(inout) :: lhs
        type(argument),  intent(in)    :: rhs

        if (allocated(rhs%chars)) lhs%chars = rhs%chars
    end subroutine

    pure subroutine argument_assign_character(lhs, rhs)
        class(argument), intent(inout) :: lhs
        character(*), intent(in)       :: rhs

        lhs%chars = rhs
    end subroutine

    pure subroutine character_assign_argument(lhs, rhs)
        character(:), allocatable, intent(inout) :: lhs
        class(argument),  intent(in)             :: rhs
        

        lhs = rhs%chars
    end subroutine
    
    function argument_trim(str) result(res)
        class(argument), intent(in) :: str
        character(:), allocatable   :: res
        
        res = trim(str%chars)
    end function
    
    integer function argument_index(str, substr, back) result(res)
        type(argument), intent(in)      :: str
        character(*), intent(in)        :: substr
        logical, optional, intent(in)   :: back
        
        res = index(str%chars, substr, back)
    end function
    
    subroutine argument_write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
        class(argument), intent(in) :: dtv !< The argument.
        integer, intent(in)         :: unit !< Logical unit.
        character(*), intent(in)    :: iotype !< Edit descriptor.
        integer, intent(in)         :: v_list(:) !< Edit descriptor list.
        integer, intent(out)        :: iostat !< IO status code.
        character(*), intent(inout) :: iomsg !< IO status message.

        if (allocated(dtv%chars)) then
            write (unit, '(A)', iostat=iostat, iomsg=iomsg) dtv%chars
        else
            write (unit, '(A)', iostat=iostat, iomsg=iomsg) ''
        end if
    end subroutine
    
    subroutine argument_read_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
        class(argument), intent(inout)    :: dtv !< The argument.
        integer, intent(in)             :: unit !< Logical unit.
        character(*), intent(in)        :: iotype !< Edit descriptor.
        integer, intent(in)             :: v_list(:) !< Edit descriptor list.
        integer, intent(out)            :: iostat !< IO status code.
        character(*), intent(inout)     :: iomsg !< IO status message.
        !private
        character(:), allocatable :: buffer
        integer :: n

        inquire(unit, size=n)
        allocate(character(len=n) :: buffer)
        read (unit, '(A)', iostat=iostat, iomsg=iomsg) buffer
        dtv%chars = buffer
    end subroutine

    subroutine argument_read_unformatted(dtv, unit, iostat, iomsg)
        class(argument), intent(inout)  :: dtv !< The argument.
        integer, intent(in)             :: unit !< Logical unit.
        integer, intent(out)            :: iostat !< IO status code.
        character(*), intent(inout)     :: iomsg !< IO status message.
        !private
        character(:), allocatable :: buffer
        integer :: n

        inquire(unit, size=n)
        allocate(character(len=n) :: buffer)
        read (unit, iostat=iostat, iomsg=iomsg) buffer
        dtv%chars = buffer
      end subroutine
  
      subroutine argument_write_unformatted(dtv, unit, iostat, iomsg)
          class(argument), intent(in)   :: dtv !! The argument.
          integer, intent(in)           :: unit !! Logical unit.
          integer, intent(out)          :: iostat !! IO status code.
          character(*), intent(inout)   :: iomsg !! IO status message.
  
          if (allocated(dtv%chars)) then
              write (unit, iostat=iostat, iomsg=iomsg) dtv%chars
          else
              write (unit, iostat=iostat, iomsg=iomsg) ''
          end if
      end subroutine
end module

#define console(name) \
program name; \
use applibrary; \
implicit none; \
type(argument), allocatable :: args(:); \
args = split_commandline_into_args(); \
if (size(args) > 1) then; \
call startup(args(2:)); \
else; \
deallocate(args); allocate(args(0));\
call startup(args); \
end if;\
contains 
#define main(args) subroutine startup(args); \
type(argument), intent(in) :: args(:); \
type(application) :: app; \
block
#define endmain end block; \
end subroutine
#define cast(var, T) \
select type (var) ;\
type is (T)
#define endcast end select