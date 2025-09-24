module modules_layout
    use fpm_model, only: fpm_model_t
    use fpm_strings, only: string_t

    implicit none; private

    type, abstract, public :: layout 
        private
    contains
        private
        procedure(generate_x), pass(this), public, deferred :: generate
    end type

    abstract interface
        subroutine generate_x(this, model, filepath, extension, exclude)
            import
            implicit none
            class(layout), intent(in)               :: this
            class(fpm_model_t), intent(inout)       :: model
            character(*), intent(in)                :: filepath
            character(*), intent(in)                :: extension
            type(string_t), optional, intent(in)    :: exclude(:)
        end subroutine
    end interface
end module