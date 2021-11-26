program main
    use :: ex_stdlib_system
    use :: ex_stdlib_type
    use :: ex_stdlib_function
    implicit none

    ! ex_stdlib_system
    ! call ex_stdlib_error_check()
    ! call ex_stdlib_error_error_stop()
    call ex_stdlib_kinds()
    call ex_stdlib_logger()
    call ex_stdlib_io()
    call ex_stdlib_system_sleep()

    ! ex_stdlib_type
    call ex_stdlib_ascii()
    call ex_stdlib_strings()
    call ex_stdlib_string_type()
    call ex_stdlib_stringlist_type()
    call ex_stdlib_bitset()

    ! ex_stdlib_function
    call ex_stdlib_optval()
    call ex_stdlib_math()
    call ex_stdlib_linalg()
    call ex_stdlib_specialfunctions()
    call ex_stdlib_quadrature()
    call ex_stdlib_random()
    call ex_stdlib_stats()
    call ex_stdlib_stats_distribution_uniform()
    call ex_stdlib_sorting()
end program main
