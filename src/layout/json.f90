module modules_layout_json
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    use modules_layout, only: layout
    use modules_utilities, only: string_contains, handle_error
    use fpm_model, only: fpm_model_t
    use fpm_strings, only: string_t
    use fpm_error, only : error_t
    use modules_submodules, only: submodule_t

    implicit none; private

    type, extends(layout), public :: json
    private
    contains
    private
    procedure, pass(this), public :: generate => generate_json
    end type

    contains

    subroutine generate_json(this, model, filepath, extension, submodules, exclude)
        class(json), intent(in)                 :: this
        class(fpm_model_t), intent(inout)       :: model
        character(*), intent(in)                :: filepath
        character(*), intent(in)                :: extension
        type(submodule_t), intent(in)           :: submodules(:)
        type(string_t), optional, intent(in)    :: exclude(:)
        !private
        type(error_t), allocatable :: error
        
        if (len_trim(filepath) == 0) then
            call model%dump(stdout, error, json=.true.)
        else
            if (extension /= '.json') then
                error = error_t('Error: Unsupported file extension '//extension//' for json layout. Supported extensions is .json')
                call handle_error(error)
                return
            end if
            call model%dump(filepath//extension, error, json=.true.)
        end if
        call handle_error(error)
    end subroutine
end module