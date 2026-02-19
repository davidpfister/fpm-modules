module modules_layout_graphviz
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit
    use modules_layout, only: layout
    use modules_utilities, only: string_contains, string_strip, handle_error
    use fpm_model, only: fpm_model_t
    use fpm_strings, only: string_t
    use fpm_error, only : error_t
    use modules_submodules, only: submodule_t, is_submodule

    implicit none; private

    type, extends(layout), public :: graphviz
    private
    character(:), allocatable :: name
    contains
    private
    procedure, pass(this), public :: generate => generate_graphviz
    end type

    type, extends(graphviz), public :: dot
    end type

    type, extends(graphviz), public :: fdp
    end type

    type, extends(graphviz), public :: sfdp
    end type

    type, extends(graphviz), public :: neato
    end type

    type, private :: self_mods
        type(string_t), allocatable :: modules(:)
    end type

    interface dot
        module procedure :: dot_new
    end interface

    interface fdp
        module procedure :: fdp_new
    end interface

    interface sfdp
        module procedure :: sfdp_new
    end interface

    interface neato
        module procedure :: neato_new
    end interface

    contains

    type(dot) function dot_new() result(that)
            that%graphviz = graphviz('dot')
    end function

    type(fdp) function fdp_new() result(that)
            that%graphviz = graphviz('fdp')
    end function

    type(sfdp) function sfdp_new() result(that)
            that%graphviz = graphviz('sfdp')
    end function

    type(neato) function neato_new() result(that)
            that%graphviz = graphviz('neato')
    end function

    subroutine generate_graphviz(this, model, filepath, extension, submodules, exclude)
        class(graphviz), intent(in)             :: this
        class(fpm_model_t), intent(inout)       :: model
        character(*), intent(in)                :: filepath
        character(*), intent(in)                :: extension
        type(submodule_t), intent(in)           :: submodules(:)
        type(string_t), optional, intent(in)    :: exclude(:)
        !private
        
        type(error_t), allocatable :: error
        character(1) :: svg
        integer :: i, iostat, n, unit, sunit
        character(100) :: iomsg
        logical :: exists, is_added

        select case (extension)
        case ('.html', '.htm')
            open(newunit=unit, file=this%name//'.gv', action='readwrite', status='replace', access='stream', form='formatted')
            call write_dot()
            close(unit)

            call execute_command_line(this%name//' -Tsvg_inline '//this%name//'.gv > '//this%name//'.svg', wait=.true., exitstat=iostat, cmdmsg=iomsg)
            if (iostat /= 0) then
                error = error_t('Error: Graphviz command '//trim(this%name)//' failed. Message: '//trim(iomsg))
                call handle_error(error)
                return
            end if

            open(newunit=unit, file=this%name//'.gv'); close(unit, status='delete') !clean up

            if (len_trim(filepath) == 0) then
                unit = stdout
            else
                open(newunit=unit, file=filepath//extension, action='write', status='replace')
            end if
            write(unit,'(*(A,/))')              &
            '<!DOCTYPE html>'               ,   &
            '<html lang="en">'              ,   &
            '<head>'                        ,   &
            '   <script src="https://cdn.jsdelivr.net/npm/svg-pan-zoom@3.6.2/dist/svg-pan-zoom.min.js"></script>', &
            '   <meta charset="utf-8">'     ,   &
            '</head>'                       ,   &
            '<body>'                        ,   &
            '    <div class="diagram-container" id="diagram-container">'
            !copy content of the svg
            inquire(file=this%name//'.svg', exist=exists)
            if (exists) then
                open(newunit=sunit, file=this%name//'.svg', status='old', access='stream')
                inquire(unit=sunit, size=n)
                do i = 1, n
                    read(sunit, iostat=iostat, pos=i) svg
                    if (iostat /= 0) exit
                    write(unit, '(A)', advance='no') svg
                end do
                close(sunit, status='delete') !clean up
            end if
            write(unit,'(*(A,/))')              &
            '   </div>'                     ,   &
            '<script>', &
            'const container = document.getElementById("diagram-container");', &
            'const svgElement = container.querySelector("svg");', &
            'window.onload = function() {', &
            '    svgPanZoom(svgElement, {', &
            '        controlIconsEnabled: true,', &
            '        fit: true,', &
            '        center: true,', &
            '        fit: false,', &
            '        zoomEnabled: true,', &
            '        panEnabled: true,', &
            '        dblClickZoomEnabled: true,', &
            '        preventEventsDefaults: true,', &
            '        minZoom: 0.1,', &
            '        maxZoom: 6,', &
            '         zoomScaleSensitivity: 0.3', &
            '    });', &
            '};', &
            '</script>'                        ,   &
            '</body>'                       ,   &
            '</html>'
        case ('.dot', '.gv', '')
            if (len_trim(filepath) == 0) then
                unit = stdout
            else
                open(newunit=unit, file=filepath//extension, action='readwrite', status='replace', access='stream', form='formatted')
            end if
            call write_dot()
        case ('.svg', '.jpg', '.png')
            open(newunit=unit, file=this%name//'.gv', action='readwrite', status='replace', access='stream', form='formatted')
            call write_dot()
            close(unit)

            if (len_trim(filepath) /= 0) then
                call execute_command_line(this%name//' -T'//extension(2:)//' '//this%name//'.gv > '//filepath//extension, wait=.true., exitstat=iostat, cmdmsg=iomsg)
            end if

            open(newunit=unit, file=this%name//'.gv'); close(unit, status='delete') !clean up
            
            if (iostat /= 0) then
                error = error_t('Error: Graphviz command '//trim(this%name)//' failed. Message: '//trim(iomsg))
                call handle_error(error)
                return
            end if
        case default
            error = error_t('Error: Unsupported file extension '//extension//' for graphviz layout. Supported extensions are .dot, .gv, .svg, .jpg, .png and .html')
            call handle_error(error)
            return
        end select

        if (unit /= stdout) close(unit)
    contains 

        subroutine write_dot()
            !private
            integer :: i, j, k, l
            type(string_t), allocatable :: excludes_mods(:)
            type(self_mods), allocatable :: smods(:)

            allocate(excludes_mods(0))
            allocate(smods(merge(size(model%packages), 0, allocated(model%packages))))

            write(unit, '(A)') 'digraph modules {'
            do i = 1, merge(size(model%packages), 0, allocated(model%packages))
                associate(s => model%packages(i)%sources)
                    allocate(smods(i)%modules(0))
                    if (present(exclude)) then
                        if (string_contains(exclude, model%packages(i)%name)) then
                            do j = 1, size(s)
                                do k = 1, size(s(j)%modules_provided)
                                    excludes_mods = [excludes_mods, s(j)%modules_provided(k)]
                                end do
                            end do
                            cycle
                        end if
                    end if
                    do j = 1, size(s)
                        do k = 1, size(s(j)%modules_provided)
                            smods(i)%modules = [smods(i)%modules, s(j)%modules_provided(k)]
                        end do
                    end do

                    write(unit,'("    subgraph cluster_", i0, " {")') i
                    write(unit,'("        ", A)') 'style=filled'
                    write(unit,'("        ", A)') 'color=lightgrey'
                    write(unit,'("        ", A)') 'node [style=filled,color=white, shape=box]'
                    write(unit,'("        label = """, A, """")') string_strip(model%packages(i)%name)
                    do j = 1, size(s)
                        do k = 1, size(s(j)%modules_provided)
                            is_added = .false.
                            do l = 1, size(s(j)%modules_used)
                                if (.not. string_contains(excludes_mods, s(j)%modules_used(l)) .and. &
                                        string_contains(smods(i)%modules, s(j)%modules_used(l)) .and. &
                                    .not. is_submodule(submodules, s(j)%modules_provided(k)%s, s(j)%modules_used(l)%s)) then
                                    write(unit,'("        ", A, " -> ", A, A)') s(j)%modules_provided(k)%s, s(j)%modules_used(l)%s, '[style="solid"]'
                                    is_added = .true.
                                end if
                            end do
                            if (merge(size(s(j)%parent_modules), 0, allocated(s(j)%parent_modules)) > 0) then
                                write(unit,'("        ", A, " -> ", A, A)') s(j)%parent_modules(1)%s, s(j)%modules_provided(k)%s, '[style="dashed"]'
                            end if
                            if (.not. is_added) then
                                write(unit,'("        ", A)') s(j)%modules_provided(k)%s
                            end if
                            exit !set all the use to belong to the first module in the file
                        end do
                    end do
                    write(unit,'(A)') '    }'
                end associate    
            end do

            do i = 1, merge(size(model%packages), 0, allocated(model%packages))
                if (present(exclude)) then; if (string_contains(exclude, model%packages(i)%name)) cycle; end if
                associate(s => model%packages(i)%sources)
                    do j = 1, size(s)
                        do k = 1, size(s(j)%modules_provided)
                            do l = 1, size(s(j)%modules_used)
                                if (.not. string_contains(excludes_mods, s(j)%modules_used(l)) .and. &
                                    .not. string_contains(smods(i)%modules, s(j)%modules_used(l))) then
                                    write(unit,'("    ", A, "->", A)') s(j)%modules_provided(k)%s, s(j)%modules_used(l)%s
                                end if
                            end do
                            exit !set all the use to belong to the first module in the file
                        end do
                    end do
                end associate
            end do
            write(unit,'(A)') '}'
        end subroutine
    end subroutine
end module