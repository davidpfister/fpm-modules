#include <assertion.inc>
TESTPROGRAM(main)
    TEST('test_json')
        use modules_packages

        type(package) :: p
        logical :: exist
        integer :: unit, s

        call new(p, 'fpm.toml', 'json')
        call p%display('test/test', '.json')

        open(newunit=unit, file='test/test.json')
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
        call p%display('test/test', '.html')

        open(newunit=unit, file='test/test.html')
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
        call p%display('test/test', '.html')

        open(newunit=unit, file='test/test.html')
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
        call p%display('test', '.html')

        open(newunit=unit, file='test.html')
        inquire(unit=unit, exist=exist, size=s)

        EXPECT_TRUE(exist)
        EXPECT_GT(s, 0)

        close(unit, status='delete')
    END_TEST
END_TESTPROGRAM