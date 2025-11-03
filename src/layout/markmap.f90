module modules_layout_markmap
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    use fpm_filesystem, only: basename
    use modules_layout, only: layout
    use modules_utilities, only: string_contains, handle_error
    use fpm_model, only: fpm_model_t
    use fpm_strings, only: string_t
    use fpm_error, only : error_t
    use fpm_filesystem, only: basename

    implicit none; private

    type, extends(layout), public :: markmap
    private
    contains
    private
    procedure, pass(this), public :: generate => generate_markmap
    end type

    contains

    subroutine generate_markmap(this, model, filepath, extension, submodules, exclude)
        class(markmap), intent(in)               :: this
        class(fpm_model_t), intent(inout)       :: model
        character(*), intent(in)                :: filepath
        character(*), intent(in)                :: extension
        type(string_t), intent(in)              :: submodules(:)
        type(string_t), optional, intent(in)    :: exclude(:)
        !private
        integer :: unit
        type(error_t), allocatable :: error


        if (len_trim(filepath) == 0) then
            unit = stdout
        else
            open(newunit=unit, file=filepath//extension, action='readwrite', status='replace', access='stream', form='formatted')
        end if

        select case (extension)
        case ('.html', '.htm')
            write(unit,'(*(A,/))')              &
            '<!DOCTYPE html>'               ,   &
            '<html lang="en">'              ,   &
            '<head>', &
            '    <meta charset="UTF-8" />', &
            '    <meta http-equiv="X-UA-Compatible" content="IE=edge" />', &
            '    <meta name="viewport" content="width=device-width, initial-scale=1.0" />', &
            '    <title>Markmap</title>', &
            '    <style>', &
            '    svg.markmap {', &
            '        width: 100%;', &
            '        height: 100vh;', &
            '    }', &
            '    </style>', &
            '    <script src="https://cdn.jsdelivr.net/npm/markmap-autoloader@0.18"></script>', &
            '</head>',   &
            '<body>'                        ,   &
            '   <div class="markmap">'      ,   &
            '       <script type="text/template">', &
            '          ---', &
            '          markmap:', &
            '            maxWidth: 300', &
            '            colorFreezeLevel: 2', &
            '          ---', &
            ''
            call write_md('packages')
            write(unit,'(*(A,/))') &
            '       </script>', &
            '    </div>', &
            '</body>', &
            '</html>'
        case ('.md','')
            call write_md('packages')
        case default
            error = error_t('Error: Unsupported file extension '//extension//' for circle layout. Supported extensions are .json and .html')
            call handle_error(error)
        end select   
        if (unit /= stdout) close(unit)
    contains 
        subroutine write_md(name)
            character(*) :: name
            !private
            integer :: i, j, k, s

            write(unit,'("          # " , A)') name // new_line('a')
            do i = 1, size(model%packages)
                if (present(exclude)) then
                    if (.not. string_contains(exclude, model%packages(i)%name)) then
                        write(unit,'("          ## ", A)') model%packages(i)%name // new_line('a')
                        do j = 1, size(model%packages(i)%sources)
                            do k = 1, size(model%packages(i)%sources(j)%modules_provided)
                                if (allocated(model%deps%dep(i)%git)) then
                                    write(unit,'("          - [", A, "](", A, ")")') &
                                        model%packages(i)%sources(j)%modules_provided(k)%s, get_url(model%deps%dep(i)%git%url, model%packages(i)%sources(j)%file_name)
                                else
                                    write(unit,'("          - [", A, "](", A, ")")') &
                                        model%packages(i)%sources(j)%modules_provided(k)%s, model%packages(i)%sources(j)%file_name
                                end if
                            end do
                        end do
                    end if
                else
                    write(unit,'("          ## ", A)') model%packages(i)%name // new_line('a')
                    do j = 1, size(model%packages(i)%sources)
                        do k = 1, size(model%packages(i)%sources(j)%modules_provided)
                            if (allocated(model%deps%dep(i)%git)) then
                                write(unit,'("          - [", A, "](", A, ")")') &
                                    model%packages(i)%sources(j)%modules_provided(k)%s, get_url(model%deps%dep(i)%git%url, model%packages(i)%sources(j)%file_name)
                            else
                                write(unit,'("          - [", A, "](", A, ")")') &
                                    model%packages(i)%sources(j)%modules_provided(k)%s, model%packages(i)%sources(j)%file_name
                            end if
                        end do
                    end do
                end if
                write(unit,'(A)') new_line('a')
            end do
        end subroutine

        function get_url(url, filepath) result(res)
            character(*), intent(in) :: url
            character(*), intent(in) :: filepath
            character(:), allocatable :: res
            !private
            integer :: i, j

            if (index(url, '.git') > 0) then
                res = url(:len_trim(url)-4)
            else
                res = url
            end if

            if (res(len(res):len(res)) == '/') res = res(:len(res)-1)

            i = index(res, '/', back = .true.)
            j = index(filepath, res(i+1:))

            res = res//'/tree/HEAD/'//filepath(j + len_trim(res(i:)):)
            do i = 1, len(res)
                if (res(i:i) == '\') res(i:i) = '/'
            end do
        end function
    end subroutine
end module