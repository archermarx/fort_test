! Part of the fort_test repository https://github.com/archermarx/fort_test
! Thomas Marks, 2020
! Published with the GPL license

program runtests
     
    use fort_test
    use iso_fortran_env

    implicit none

    type(TestSet):: logical_tests, integer_tests, real_tests, string_tests
    type(TestSet), dimension(:), allocatable:: tests

    logical_tests = new_testset(    &
        (/  &
            assert_eq((2 + 2 == 4), (4 + 4 == 8)), &
            assert_neq((2 + 2 == 4), (4 + 3 == 8)) &
        /), &
        name = "Logical type tests" &
    )
    
    integer_tests = new_testset(    &
        (/ &
            assert_eq(10_int32, 10_int32),      &
            assert_eq(20_int64, 30_int64),      &

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
    call print_and_exit(tests)
end program