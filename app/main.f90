# ifdef __GFORTRAN__
#    define MACRO_SAME(A) A
#    define MACRO_STRINGIFY(A) "A"
# else
#    define MACRO_SAME(A) A
#    define MACRO_STRINGIFY(A) #A
# endif
#include <app.inc>
console(main)
    main(args)
        use fpm_filesystem, only : join_path
        use fpm_strings, only: string_t
        use modules_packages
        use modules_utilities

        use, intrinsic :: iso_fortran_env, only: stdout => output_unit, &
                            stderr => error_unit
        

        integer :: i, j, k, l, nargs, pos
        character(:), allocatable :: dir !< path to directory containing the fpm.toml file
        character(:), allocatable :: tomlfile
        character(:), allocatable :: layout
        character(:), allocatable :: output, filepath, extension
        type(string_t), allocatable :: exclude(:)
        type(package) :: p
#if defined(_VERSION)
       character(*), parameter :: version = _VERSION
#else
       character(*), parameter :: version = '0.0.0'
#endif

        nargs = size(args)
        i = 1

        !default
        layout = 'dot'
        dir = './'
        allocate(exclude(0))

        do while (i <= nargs)
            select case(args(i)%chars)
            case ('-d', '--dir')
                i = i + 1
                if (i <= nargs) dir = args(i)
            case ('-o', '--output')
                i = i + 1
                if (i <= nargs) output = args(i) 
            case ('-x','--exclude')
                i = i + 1
                if (i <= nargs) then
                    j = 0
                    k = 1
                    l = len(args(i)%chars)
                    do while (j < l)
                        j = j + 1
                        if (args(i)%chars(j:j) == '"') cycle
                        if (args(i)%chars(j:j) == ',') then
                            exclude = [exclude, string_t(trim(args(i)%chars(k:j-1)))]
                            k = j + 1
                        end if
                    end do
                    exclude = [exclude, string_t(trim(args(i)%chars(k:merge(l-1, l, args(i)%chars(l:l)=='"'))))]
                end if
            case ('-K','--layout')
                i = i + 1
                if (i <= nargs) layout = args(i)
            case ('-v','--version')
                i = i + 1
                write(*, '(*(A,/))') 'fpm-modules version '//version,     &
                                     'Copyright (C) 2025 davidpfister', &
                                     'This is free software; see the source for copying conditions.  There is NO', &
                                     'warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.'
                stop
            case ('-h','--help', '?')
                i = i + 1
                write(*, '(*(A,/))') '                             fpm-modules plugin help', &
                                     '                             =======================', &
                                     'fpm-modules is a simple plugin for modern Fortran to generate module dependency graphs.', &
                                     '', &
                                     '                             Plugin Option List', &
                                     '                             --------------------', &
                                     '-K, --layout      Selection of the layout engine. Supported values are "mermaid", "force",', & 
                                     '                  "dot", "fdp", "sfdp", "neato", "markmap", and "circle"', &
                                     '-d, --dir         Directory containing the fpm compatible toml file at the root', &
                                     '                  of the project. By default it uses the current working directory.', &
                                     '                  The directory can also be passed by position, assuming the first', &
                                     '                  position is used.', &
                                     '-h, --help, ?     Display the help.', &
                                     '-o, --output      Output file path with name and extension.', &
                                     '-v, --version     Display the version of the program.', &
                                     '-x, --exclude     Comma separated list of excluded packages.'
                stop
            case default
                if (i == 1) dir = args(i)
            end select
            i = i + 1
        end do

        if (allocated(output)) then
            pos = index(output, '.', back=.true.)
            filepath = fullpath(output(:pos-1))
            extension = output(pos:)
        else
            filepath = ''
            extension = ''
        end if

        call chdir(dir)

        tomlfile = join_path('', 'fpm.toml')
        call new(p, tomlfile, layout)

        call p%display(filepath, extension, exclude)

    endmain
end
