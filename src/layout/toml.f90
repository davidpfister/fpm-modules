module modules_layout_toml
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    use modules_layout, only: layout
    use modules_utilities, only: string_contains, handle_error
    use fpm_model, only: fpm_model_t
    use fpm_strings, only: string_t
    use fpm_error, only : error_t
    use modules_submodules, only: submodule_t

    implicit none; private

    type, extends(layout), public :: toml
    private
    contains
    private
    procedure, pass(this), public :: generate => generate_toml
    end type

    contains

    subroutine generate_toml(this, model, filepath, extension, submodules, exclude)
        class(toml), intent(in)                 :: this
        class(fpm_model_t), intent(inout)       :: model
        character(*), intent(in)                :: filepath
        character(*), intent(in)                :: extension
        type(submodule_t), intent(in)           :: submodules(:)
        type(string_t), optional, intent(in)    :: exclude(:)
        !private
        type(error_t), allocatable :: error
        
        if (len_trim(filepath) == 0) then
            call model%dump(stdout, error, json=.false.)
        else
            if (extension /= '.toml') then
                error = error_t('Error: Unsupported file extension '//extension//' for toml layout. Supported extensions is .toml')
                call handle_error(error)
                return
            end if
            call model%dump(filepath//extension, error, json=.false.)
        end if
        call handle_error(error)
    end subroutine
end module