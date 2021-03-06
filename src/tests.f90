! Part of the fort_test repository https://github.com/archermarx/fort_test
! Thomas Marks, 2020
! Published with the GPL license

program runtests
     
    use fort_test
    use iso_fortran_env

    implicit none

    integer:: num_failed
    type(TestSet):: logical_tests, integer_tests, real_tests, string_tests, failure_tests
    type(TestSet), dimension(:), allocatable:: tests

    logical_tests = new_testset(    &
        (/  &
            assert(.true.) ,    &
            assert_eq((2 + 2 == 4), (4 + 4 == 8)), &
            assert_neq((2 + 2 == 4), (4 + 3 == 8)) &
        /), &
        name = "Logical type tests" &
    )
    
    integer_tests = new_testset(    &
        (/ &
            assert_eq(10_int32, 10_int32),      &
            assert_eq(20_int64, 20_int64),      &

            assert_neq(10_int32, 20_int32),     &
            assert_neq(10_int64, 20_int64),     &

            assert_positive(10_int32),  &
            assert_positive(10_int64),  &

            assert_negative(-10_int32),  &
            assert_negative(-10_int64),  &

            assert_gt(20_int32, 10_int32),  &
            assert_gt(20_int64, 10_int64),  &

            assert_geq(20_int32, 10_int32), &
            assert_geq(20_int32, 20_int32), &
            assert_geq(20_int64, 10_int64), &
            assert_geq(20_int64, 20_int64), &

            assert_lt(10_int32, 20_int32),  &
            assert_lt(10_int64, 20_int64),  &

            assert_leq(10_int32, 20_int32), &
            assert_leq(20_int32, 20_int32), &
            assert_leq(10_int64, 20_int64), &
            assert_leq(20_int64, 20_int64)  &
        /),  &
        name = "Integer tests"      &
    )

    real_tests = new_testset(    &
        (/ &
            assert_eq(10.0, 10.0),      &
            assert_eq(10.0d0, 10.0d0),      &

            assert_neq(10.0, 20.0),     &
            assert_neq(10.0d0, 20.0d0),     &

            assert_positive(10.0),  &
            assert_positive(10.0d0),  &

            assert_negative(-10.0),  &
            assert_negative(-10.0d0),  &

            assert_gt(20.0, 10.0),  &
            assert_gt(20.0d0, 10.0d0),  &

            assert_geq(20.0, 10.0), &
            assert_geq(20.0, 20.0), &
            assert_geq(20.0d0, 10.0d0), &
            assert_geq(20.0d0, 20.0d0), &

            assert_lt(10.0, 20.0),  &
            assert_lt(10.0d0, 20.0d0),  &

            assert_leq(10.0, 20.0), &
            assert_leq(20.0, 20.0), &
            assert_leq(10.0d0, 20.0d0), &
            assert_leq(20.0d0, 20.0d0),  &

            assert_approx(1.0, 1.0 + 10*epsilon(1.0)), &
            assert_approx(1.0d0, 1.0d0 + 10 * epsilon(1.0d0))   &
        /),  &
        name = "Real tests"      &
    )

    string_tests = new_testset(    &
        (/          &
            assert_eq("Cheese", "Cheese"),      &
            assert_neq("Cheese", "Pizza"),      &
            assert_eq("Cheese", "Cheese    "),  &
            assert_neq("  Cheese", "Cheese"),   &
            assert_neq("Cheese", "cheese")      &
        /),          &
        name = "String tests"   &
    )

    tests = (/ logical_tests, integer_tests, real_tests, string_tests /)

    failure_tests = new_testset(    &
        (/  &
            assert(.false.), &
            assert_eq(2.0d0, 3.0d0), &
            assert_geq(2.0, 3.0), &
            assert_gt(3_int32, 4_int32), &
            assert_neq(.false., .false.), &
            assert_positive(-1_int64), &
            assert_negative(2.0), &
            assert_approx(2.0d0, 3.0d0), &
            assert_neq("Cheese", "Cheese"), &
            assert_eq("Cheese", "Pizza"), &
            assert_lt(4.0, 1.0), &
            assert_leq(-320_int64, -500_int64) &
        /), &
        "Failures"  &
    )

    write(*, *) "Running expected failures..."
    num_failed = run_all((/failure_tests, new_testset((/assert_eq(2, 3)/))/))
    
    if (num_failed .ne. size(failure_tests%test_list) + 1) then
        write(error_unit, *) "Not all expected failures failed"
        call exit(1)
    endif

    write(*, *) NEW_LINE('a')//"Running actual tests..."
    call run_and_exit(tests)
end program