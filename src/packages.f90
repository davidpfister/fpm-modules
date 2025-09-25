module modules_packages
    use fpm_strings, only: string_t
    use fpm_command_line, only : fpm_build_settings, get_command_line_settings, get_fpm_env
    use fpm_dependency, only : dependency_tree_t, new_dependency_tree
    use fpm_error, only : error_t, fpm_stop
    use fpm_filesystem, only : join_path
    use fpm_manifest, only : package_config_t, get_package_data
    use fpm_model, only: fpm_model_t
    use fpm, only: build_model
    use modules_utilities
    use modules_layouts

    implicit none; private

    public :: new

    type, extends(package_config_t), public :: package
        private
        type(fpm_model_t), public           :: model
        class(layout), allocatable          :: l
    contains
        private
        procedure, pass(this)         :: create => package_create
        procedure, pass(this), public :: display => package_display
    end type

    interface new
        module procedure :: package_new_from_file
    end interface

    contains

    subroutine package_new_from_file(pack, file, layout)
        class(package), intent(out)         :: pack
        character(*), intent(in), optional  :: file
        character(*), intent(in)            :: layout
        !private
        type(error_t), allocatable :: error
        character(:), allocatable :: filepath
        

        if (present(file)) then
            filepath = file
        else
            filepath = 'fpm.toml'
        end if
        call get_package_data(pack%package_config_t, filepath, error, apply_defaults=.true.)
        call handle_error(error)

        call pack%create(layout)
    end subroutine

    subroutine package_create(this, layout)
        class(package), intent(inout)   :: this
        character(*), intent(in)        :: layout
        !private
        type(fpm_build_settings) :: settings
        type(error_t), allocatable :: error

        settings = fpm_build_settings(  &
        &   profile=" ",&
        &   dump='fpm_model.toml',&
        &   prune= .false., &
        &   compiler=get_fpm_env("FC", "gfortran"), &
        &   c_compiler=get_fpm_env("CC", " "), &
        &   cxx_compiler= get_fpm_env("CXX", " "), &
        &   archiver= get_fpm_env("AR", " "), &
        &   path_to_config= " ", &
        &   flag=" ", &
        &   cflag=" ", &
        &   cxxflag=" ", &
        &   ldflag=" ", &
        &   list=.false.,&
        &   show_model=.false.,&
        &   build_tests=.false.,&
        &   verbose=.false.)

        call build_model(this%model, settings, this%package_config_t, error)
        if (allocated(error)) then
            call fpm_stop(1,'*package_build* Model error: '//error%message)
        end if

        select case(layout)
        case('mermaid')
            allocate(this%l, source = mermaid())
        case('force')
            allocate(this%l, source = force())
        case('dot')
            allocate(this%l, source = dot())
        case('fpd')
            allocate(this%l, source = fdp())
        case('sfdp')
            allocate(this%l, source = sfdp())
        case('neato')
            allocate(this%l, source = neato())
        case('circle')
            allocate(this%l, source = circle())
        case('json')
            allocate(this%l, source = json())
        case('toml')
            allocate(this%l, source = toml())
        case default
            call fpm_stop(1,'Unknown layout option. Supported values are "mermaid", "force", "dot", "fdp", "sfdp", "neato", "circle" and "json"')
        end select
    end subroutine

    subroutine package_display(this, filepath, extension, exclude)
        class(package), intent(inout)       :: this
        character(*), intent(in)            :: filepath
        character(*), intent(in)            :: extension
        type(string_t), optional, intent(in):: exclude(:)

        call this%l%generate(this%model, filepath, extension, exclude)
    end subroutine

end module
