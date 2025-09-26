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

#ifdef __GFORTRAN__
        !> Determine the absolute, canonicalized path for a given path.
        !> Calls custom C routine because the `_WIN32` macro is correctly exported
        !> in C using `gfortran`.
        type(c_ptr) function c_realpath(path, resolved_path, maxLength) bind(C, name="c_realpath")
        import c_char, c_int, c_ptr
            character(kind=c_char, len=1), intent(in)   :: path(*)
            character(kind=c_char, len=1), intent(out)  :: resolved_path(*)
            integer(c_int), value, intent(in)           :: maxLength
        end function
#else
#ifndef _WIN32
        function realpath_c(path, resolved_path) result(ptr) bind(C, name="realpath")
            import :: c_ptr, c_char
            character(kind=c_char, len=1), intent(in) :: path(*)
            character(kind=c_char, len=1), intent(out) :: resolved_path(*)
            type(c_ptr) :: ptr
        end function
#else
        function fullpath_c(resolved_path, path, maxLength) result(ptr) bind(C, name="_fullpath")
            import :: c_ptr, c_char, c_int
            character(kind=c_char, len=1), intent(out) :: resolved_path(*)
            character(kind=c_char, len=1), intent(in) :: path(*)
            integer(c_int), value, intent(in) :: maxLength
            type(c_ptr) :: ptr
        end function
#endif
#endif
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
        !private
        type(c_ptr) :: ptr
        integer, parameter :: MAX_PATH = 256
        character(1) :: tmp(MAX_PATH)
        integer idx

        allocate (character(MAX_PATH) :: resolved_path)
#ifdef __GFORTRAN__
        ptr = c_realpath(path//c_null_char, tmp, MAX_PATH)
#else
#ifndef _WIN32
        ptr = realpath_c(path//c_null_char, tmp)
#else
        ptr = fullpath_c(tmp, path//c_null_char, MAX_PATH)
#endif
#endif
        resolved_path = transfer(tmp, resolved_path)
        idx = index(resolved_path, c_null_char)
        resolved_path = resolved_path(:idx - 1)
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