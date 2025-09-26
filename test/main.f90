#include <assertion.inc>
TESTPROGRAM(main)
    TEST('test_json')
        use modules_packages

        type(package) :: p
        logical :: exist
        integer :: unit, s

        call new(p, 'fpm.toml', 'json')
        call p%display('test/test_json', '.json')

        open(newunit=unit, file='test/test_json.json')
        inquire(unit=unit, exist=exist, size=s)

        EXPECT_TRUE(exist)
        EXPECT_GT(s, 0)

        close(unit, status='delete')
    END_TEST

    TEST('test_toml')
        use modules_packages

        type(package) :: p
        logical :: exist
        integer :: unit, s

        call new(p, 'fpm.toml', 'toml')
        call p%display('test/test_toml', '.toml')

        open(newunit=unit, file='test/test_toml.toml')
        inquire(unit=unit, exist=exist, size=s)

        EXPECT_TRUE(exist)
        EXPECT_GT(s, 0)

        close(unit, status='delete')
    END_TEST

    TEST('test_circle')
        use modules_packages
        
        type(package) :: p
        logical :: exist
        integer :: unit, s

        call new(p, 'fpm.toml', 'circle')
        call p%display('test/test_circle', '.html')

        open(newunit=unit, file='test/test_circle.html')
        inquire(unit=unit, exist=exist, size=s)

        EXPECT_TRUE(exist)
        EXPECT_GT(s, 0)

        close(unit, status='delete')
    END_TEST

    TEST('test_force')
        use modules_packages
        
        type(package) :: p
        logical :: exist
        integer :: unit, s

        call new(p, 'fpm.toml', 'force')
        call p%display('test/test_force', '.html')

        open(newunit=unit, file='test/test_force.html')
        inquire(unit=unit, exist=exist, size=s)

        EXPECT_TRUE(exist)
        EXPECT_GT(s, 0)

        close(unit, status='delete')
    END_TEST

    TEST('test_mermaid')
        use modules_packages
        
        type(package) :: p
        logical :: exist
        integer :: unit, s

        call new(p, 'fpm.toml', 'mermaid')
        call p%display('test/test_mermaid', '.html')

        open(newunit=unit, file='test/test_mermaid.html')
        inquire(unit=unit, exist=exist, size=s)

        EXPECT_TRUE(exist)
        EXPECT_GT(s, 0)

        close(unit, status='delete')
    END_TEST

    TEST('test_dot')
        use modules_packages
        
        type(package) :: p
        logical :: exist
        integer :: unit, s

        call new(p, 'fpm.toml', 'dot')
        call p%display('test/test_dot', '.dot')

        open(newunit=unit, file='test/test_dot.dot')
        inquire(unit=unit, exist=exist, size=s)

        EXPECT_TRUE(exist)
        EXPECT_GT(s, 0)

        close(unit, status='delete')
    END_TEST
END_TESTPROGRAM