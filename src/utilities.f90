module modules_utilities
    use, intrinsic :: iso_c_binding
    use fpm_strings, only: string_t
    use fpm_error, only : error_t

    implicit none; private

    public :: chdir,            &
              string_contains,  &
              string_strip,     &
              handle_error,     &
              fullpath

    interface
        integer function chdir_c(path) bind(c, name="chdir")
        import c_char
            character(kind=c_char) :: path(*)
        end function
    end interface

    interface string_contains
        module procedure :: string_contains_string,     &
                            string_contains_character
    end interface

    contains

    subroutine chdir(path, ierr)
        character(*), intent(in)        :: path
        integer, optional, intent(out)  :: ierr
        integer :: loc_err

        loc_err =  chdir_c(path//c_null_char)

        if (present(ierr)) ierr = loc_err
    end subroutine

    function string_strip(instr) result(res)
        character(*), intent(in) :: instr
        character(:), allocatable :: res
        !private
        integer :: i

        res = instr
        do i = 1, len(res)
            if (res(i:i) == '-') res(i:i) = '_'
        end do
    end function

    logical function string_contains_string(lhs, rhs) result(res)
        type(string_t), intent(in) :: lhs(:)
        type(string_t), intent(in) :: rhs
        !private
        integer :: i

        res = .false.
        do i = 1, size(lhs)
            if (lhs(i)%s == rhs%s) then
                res = .true.
                exit
            end if
        end do
    end function

    logical function string_contains_character(lhs, rhs) result(res)
        type(string_t), intent(in)  :: lhs(:)
        character(*), intent(in)    :: rhs
        !private
        integer :: i

        res = .false.
        do i = 1, size(lhs)
            if (lhs(i)%s == rhs) then
                res = .true.
                exit
            end if
        end do
    end function

    function fullpath(path) result(resolved_path)
        character(*), intent(in) :: path
        character(:), allocatable :: resolved_path

        resolved_path = trim(adjustl(workdir() // path))
    end function

    function workdir() result(path)
        character(:), allocatable :: path

        allocate (character(256) :: path)
        call getcwd(path)
        path = trim(adjustl(path))
    end function

    subroutine handle_error(err)
        type(error_t), optional, intent(in) :: err
        if (present(err)) then
            write (*, '("[Error]", 1x, a)') err%message
            stop 1
        end if
    end subroutine
end module