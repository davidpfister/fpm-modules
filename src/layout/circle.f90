module modules_layout_circle
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    use fpm_filesystem, only: basename
    use modules_layout, only: layout
    use modules_utilities, only: string_contains, handle_error
    use fpm_model, only: fpm_model_t
    use fpm_strings, only: string_t
    use fpm_error, only : error_t
    use fpm_filesystem, only: basename

    implicit none; private

    type, extends(layout), public :: circle
    private
    contains
    private
    procedure, pass(this), public :: generate => generate_circle
    end type

    contains

    subroutine generate_circle(this, model, filepath, extension, exclude)
        class(circle), intent(in)               :: this
        class(fpm_model_t), intent(inout)       :: model
        character(*), intent(in)                :: filepath
        character(*), intent(in)                :: extension
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
            '<head>'                        ,   &
            '   <style> body { margin: 0; } </style>'     ,   &
            '   <script src="https://cdn.jsdelivr.net/npm/circlepack-chart"></script>'     ,   &
            '</head>'                       ,   &
            '<body>'                        ,   &
            '   <div id="graph"></div>'      ,   &
            '       <script type="module">', &
            '       import { scaleOrdinal } from "https://esm.sh/d3-scale";', &
            '       import { schemePaired } from "https://esm.sh/d3-scale-chromatic";'
            if (unit == stdout) then
                call write_json('data')
            else
                call write_json(basename(trim(filepath)))
            end if
            write(unit,'(*(A,/))') &
            '       var data = JSON.parse(datastr);', &
            '       const color = scaleOrdinal(schemePaired);', &
            '       const Graph = new CirclePack(document.getElementById("graph"))', &
            '       .data(data)', &
            '       .color(d => color(d.name))', &
            '       .minCircleRadius(8);', &
            '       </script>', &
            '   </body>', &
            '</html>'
        case ('.json','')
            call write_json(basename(trim(filepath)))
        case default
            error = error_t('Error: Unsupported file extension '//extension//' for circle layout. Supported extensions are .json and .html')
            call handle_error(error)
        end select   
        if (unit /= stdout) close(unit)
    contains 
        subroutine write_json(name)
            character(*), intent(in) :: name
            !private
            integer :: i, j, k, s

            write(unit,'(A)', advance='no') "       var datastr = '{"
            write(unit,'("""name"": """, A, """,")', advance='no') name
            write(unit,'(A)', advance='no') '"children": ['
            do i = 1, size(model%packages)
                if (present(exclude)) then
                    if (.not. string_contains(exclude, model%packages(i)%name)) then
                        write(unit,'("{""name"": """, A ,""", ""children"": [")', advance='no') model%packages(i)%name
                        do j = 1, size(model%packages(i)%sources)
                            inquire(file=model%packages(i)%sources(j)%file_name, size=s)
                            do k = 1, size(model%packages(i)%sources(j)%modules_provided)
                                write(unit,'("{""name"": """, A ,""", ""value"": """, i0 ,"""},")', advance='no') &
                                    model%packages(i)%sources(j)%modules_provided(k)%s, s
                            end do
                        end do
                        if (unit == stdout) then
                            write(unit,'(A)', advance='no') char(8)
                        else
                            inquire(unit=unit, pos=s); read(unit,'(A)', advance='no', pos=s-1)
                        end if
                        write(unit,'(A)', advance='no') ']},'
                    end if
                else
                    write(unit,'("{""name"": """, A ,""", ""children"": [")', advance='no') model%packages(i)%name
                    do j = 1, size(model%packages(i)%sources)
                        inquire(file=model%packages(i)%sources(j)%file_name, size=s)
                        do k = 1, size(model%packages(i)%sources(j)%modules_provided)
                            write(unit,'("{""name"": """, A ,""", ""value"": """, i0 ,"""},")', advance='no') &
                                model%packages(i)%sources(j)%modules_provided(k)%s, s
                        end do
                    end do
                    if (unit == stdout) then
                        write(unit,'(A)', advance='no') char(8)
                    else
                        inquire(unit=unit, pos=s); read(unit,'(A)', advance='no', pos=s-1)
                    end if
                    write(unit,'(A)', advance='no') ']},'
                end if
                
            end do
            if (unit == stdout) then
                write(unit,'(A)', advance='no') char(8)
            else
                inquire(unit=unit, pos=s); read(unit,'(A)', advance='no', pos=s-1)
            end if
            write(unit,'(A)', advance='no') ']'
            write(unit,'(A)') "}';"
        end subroutine
    end subroutine
end module