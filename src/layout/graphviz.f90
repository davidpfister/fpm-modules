module modules_layout_graphviz
    use modules_layout, only: layout
    use modules_utilities, only: string_contains, string_strip
    use fpm_model, only: fpm_model_t
    use fpm_strings, only: string_t
    use fpm_error, only : error_t

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

    subroutine generate_graphviz(this, model, filepath, exclude)
        class(graphviz), intent(in)             :: this
        class(fpm_model_t), intent(inout)       :: model
        character(*), intent(in)                :: filepath
        type(string_t), optional, intent(in)    :: exclude(:)
        !private
        type(string_t), allocatable :: excludes_mods(:)
        type(self_mods), allocatable :: smods(:)
        type(error_t), allocatable :: error
        character(1) :: svg
        integer :: i, j, k, l, n, unit, sunit
        integer :: iostat
        character(100) :: iomsg
        logical :: exists, is_added

        allocate(excludes_mods(0))
        allocate(smods(size(model%packages)))
        

        open(newunit=unit, file=this%name//'.dot', action='write', status='replace')
        write(unit, '(A)') 'digraph modules {'
        do i = 1, size(model%packages)
            allocate(smods(i)%modules(0))
            if (present(exclude)) then
                if (string_contains(exclude, model%packages(i)%name)) then
                    do j = 1, size(model%packages(i)%sources)
                        do k = 1, size(model%packages(i)%sources(j)%modules_provided)
                            excludes_mods = [excludes_mods, model%packages(i)%sources(j)%modules_provided(k)]
                        end do
                    end do
                    cycle
                end if
            end if
            do j = 1, size(model%packages(i)%sources)
                do k = 1, size(model%packages(i)%sources(j)%modules_provided)
                    smods(i)%modules = [smods(i)%modules, model%packages(i)%sources(j)%modules_provided(k)]
                end do
            end do

            write(unit,'("    subgraph cluster_", i0, " {")') i
            write(unit,'("        ", A)') 'style=filled'
            write(unit,'("        ", A)') 'color=lightgrey'
            write(unit,'("        ", A)') 'node [style=filled,color=white]'
            write(unit,'("        label = """, A, """")') string_strip(model%packages(i)%name)
            do j = 1, size(model%packages(i)%sources)
                do k = 1, size(model%packages(i)%sources(j)%modules_provided)
                    is_added = .false.
                    do l = 1, size(model%packages(i)%sources(j)%modules_used)
                        if (.not. string_contains(excludes_mods, model%packages(i)%sources(j)%modules_used(l)) .and. &
                                  string_contains(smods(i)%modules, model%packages(i)%sources(j)%modules_used(l))) then
                            write(unit,'("        ", A, " -> ", A, A)') model%packages(i)%sources(j)%modules_provided(k)%s, model%packages(i)%sources(j)%modules_used(l)%s, '[style="dashed"]'
                            is_added = .true.
                        end if
                    end do
                    if (.not. is_added) then
                        write(unit,'("        ", A)') model%packages(i)%sources(j)%modules_provided(k)%s
                    end if
                    exit !set all the use to belong to the first module in the file
                end do
            end do
            write(unit,'(A)') '    }'
        end do

        do i = 1, size(model%packages)
            if (present(exclude)) then; if (string_contains(exclude, model%packages(i)%name)) cycle; end if
            do j = 1, size(model%packages(i)%sources)
                do k = 1, size(model%packages(i)%sources(j)%modules_provided)
                    do l = 1, size(model%packages(i)%sources(j)%modules_used)
                        if (.not. string_contains(excludes_mods, model%packages(i)%sources(j)%modules_used(l)) .and. &
                            .not. string_contains(smods(i)%modules, model%packages(i)%sources(j)%modules_used(l))) then
                            write(unit,'("    ", A, "->", A)') model%packages(i)%sources(j)%modules_provided(k)%s, model%packages(i)%sources(j)%modules_used(l)%s
                        end if
                    end do
                    exit !set all the use to belong to the first module in the file
                end do
            end do
        end do
        write(unit,'(A)') '}'
        close(unit)

        call execute_command_line(this%name//' -Tsvg_inline '//this%name//'.dot > '//this%name//'.svg', wait=.true., exitstat=iostat, cmdmsg=iomsg)
        if (iostat /= 0) print*, iomsg

        open(newunit=unit, file=filepath, action='write', status='replace')
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
            close(sunit)
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

        close(unit)
    end subroutine
end module