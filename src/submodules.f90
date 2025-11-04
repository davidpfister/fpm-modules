module modules_submodules
    use fpm_model, only: fpm_model_t

    implicit none; private

    public ::   build_submodules,     &
                is_submodule

    type, public :: submodule_t
        character(:), allocatable :: name
        character(:), allocatable :: parent
    end type

    contains

    function build_submodules(model) result(res)
        type(fpm_model_t), intent(in) :: model
        type(submodule_t), allocatable :: res(:)
        !private 
        integer :: i, j, k

        allocate(res(0))

        do i = 1, size(model%packages)
            do j = 1, size(model%packages(i)%sources)
                if (merge(size(model%packages(i)%sources(j)%parent_modules), 0, allocated(model%packages(i)%sources(j)%parent_modules)) > 0) then
                    do k = 1, size(model%packages(i)%sources(j)%modules_provided)
                        res = [res, submodule_t(model%packages(i)%sources(j)%modules_provided(k)%s, model%packages(i)%sources(j)%parent_modules(k)%s)]
                    end do
                end if
            end do
        end do
    end function

    function is_submodule(that, name, parent) result(res)
        type(submodule_t), intent(in)   :: that(:)
        character(*), intent(in)        :: name
        character(*), intent(in)        :: parent
        logical :: res
        !private
        integer :: i

        res = .false.
        do i = 1, size(that)
            if (that(i)%name == name) then
                if (that(i)%parent == parent) then
                    res = .true.
                    exit
                end if
            end if
        end do
    end function
end module