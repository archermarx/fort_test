! Part of the fort_test repository https://github.com/archermarx/fort_test
! Thomas Marks, 2020
! Published with the GPL license

program runtests

    use fort_test
    use iso_fortran_env

    implicit none

    integer:: num_failed
    type(TestSet):: logical_tests, integer_tests, real_tests, string_tests, array_tests, failure_tests
    type(TestSet), dimension(:), allocatable:: tests

    logical_tests = new_testset(    &
        (/  &
            assert(logical_to_int(.true.) == 1), &
            assert(logical_to_int(.false.) == 0), &
            assert(.true.) ,    &
            assert_eq((2 + 2 == 4), (4 + 4 == 8)), &
            assert_neq((2 + 2 == 4), (4 + 3 == 8)) &
        /), &
        name = "Logical type tests" &
    )

    integer_tests = new_testset(    &
        (/ &
            assert_eq(10_int8, 10_int8),      &
            assert_eq(10_int16, 10_int16),      &
            assert_eq(10_int32, 10_int32),      &
            assert_eq(20_int64, 20_int64),      &

            assert_neq(10_int8, 20_int8),     &
            assert_neq(10_int16, 20_int16),     &
            assert_neq(10_int32, 20_int32),     &
            assert_neq(10_int64, 20_int64),     &

            assert_gt(20_int8, 10_int8),  &
            assert_gt(20_int16, 10_int16),  &
            assert_gt(20_int32, 10_int32),  &
            assert_gt(20_int64, 10_int64),  &

            assert_geq(20_int8, 10_int8), &
            assert_geq(20_int8, 20_int8), &
            assert_geq(20_int16, 10_int16), &
            assert_geq(20_int16, 20_int16), &
            assert_geq(20_int32, 10_int32), &
            assert_geq(20_int32, 20_int32), &
            assert_geq(20_int64, 10_int64), &
            assert_geq(20_int64, 20_int64), &

            assert_lt(10_int8, 20_int8),  &
            assert_lt(10_int16, 20_int16),  &
            assert_lt(10_int32, 20_int32),  &
            assert_lt(10_int64, 20_int64),  &

            assert_leq(10_int8, 20_int8), &
            assert_leq(20_int8, 20_int8), &
            assert_leq(10_int16, 20_int16), &
            assert_leq(20_int16, 20_int16), &
            assert_leq(10_int32, 20_int32), &
            assert_leq(20_int32, 20_int32), &
            assert_leq(10_int64, 20_int64), &
            assert_leq(20_int64, 20_int64)  &
        /),  &
        name = "Integer tests"      &
    )

    real_tests = new_testset(    &
        (/ &
            assert_eq(10.0_real32, 10.0_real32),      &
            assert_eq(10.0_real64, 10.0_real64),      &
            assert_eq(10.0_real128, 10.0_real128),    &

            assert_neq(10.0_real32, 20.0_real32),     &
            assert_neq(10.0_real64, 20.0_real64),     &
            assert_neq(10.0_real128, 20.0_real128),    &

            assert_gt(20.0_real32, 10.0_real32),  &
            assert_gt(20.0_real64, 10.0_real64),  &
            assert_gt(20.0_real128, 10.0_real128),  &

            assert_geq(20.0_real32, 10.0_real32), &
            assert_geq(20.0_real32, 20.0_real32), &
            assert_geq(20.0_real64, 10.0_real64), &
            assert_geq(20.0_real64, 20.0_real64), &
            assert_geq(20.0_real128, 10.0_real128), &
            assert_geq(20.0_real128, 20.0_real128), &

            assert_lt(10.0_real32, 20.0_real32),  &
            assert_lt(10.0_real64, 20.0_real64),  &
            assert_lt(10.0_real128, 20.0_real128),  &

            assert_leq(10.0_real32, 20.0_real32), &
            assert_leq(20.0_real32, 20.0_real32), &
            assert_leq(10.0_real64, 20.0_real64), &
            assert_leq(20.0_real64, 20.0_real64),  &
            assert_leq(10.0_real128, 20.0_real128), &
            assert_leq(20.0_real128, 20.0_real128),  &

            assert_approx(1.0_real32, 1.0_real32 + 10 * epsilon(1.0_real32)),   &
            assert_approx(1.0_real64, 1.0_real64 + 10 * epsilon(1.0_real64)),   &
            assert_approx(1.0_real128, 1.0_real128 + 10 * epsilon(1.0_real128)),   &

            assert_approx(1.0_real32, 1.0_real32 + 10 * epsilon(1.0_real32), &
                atol = 0.0_real32, rtol = sqrt(epsilon(1.0_real32))),       &
            assert_approx(1.0_real64, 1.0_real64 + 10 * epsilon(1.0_real64), &
                atol = 0.0_real64, rtol = sqrt(epsilon(1.0_real64))),       &
            assert_approx(1.0_real128, 1.0_real128 + 10 * epsilon(1.0_real128), &
                atol = 0.0_real128, rtol = sqrt(epsilon(1.0_real128)))   &
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

    array_tests = new_testset ( &
        (/ &
            assert_eq((/1.0_real32, 1.0_real32, 1.0_real32/), (/1.0_real32, 1.0_real32, 1.0_real32/)), &
            assert_neq((/2.0_real32, 1.0_real32, 1.0_real32/), (/1.0_real32, 1.0_real32, 1.0_real32/)), &
            assert_neq((/2.0_real32, 1.0_real32, 1.0_real32/), (/1.0_real32, 1.0_real32/)), &
            assert_eq((/1.0_real64, 1.0_real64, 1.0_real64/), (/1.0_real64, 1.0_real64, 1.0_real64/)), &
            assert_neq((/2.0_real64, 1.0_real64, 1.0_real64/), (/1.0_real64, 1.0_real64, 1.0_real64/)), &
            assert_neq((/2.0_real64, 1.0_real64, 1.0_real64/), (/1.0_real64, 1.0_real64/)), &
            assert_eq((/1.0_real128, 1.0_real128, 1.0_real128/), (/1.0_real128, 1.0_real128, 1.0_real128/)), &
            assert_neq((/2.0_real128, 1.0_real128, 1.0_real128/), (/1.0_real128, 1.0_real128, 1.0_real128/)), &
            assert_neq((/2.0_real128, 1.0_real128, 1.0_real128/), (/1.0_real128, 1.0_real128/)), &
            assert_eq((/1_int8, 1_int8, 1_int8/), (/1_int8, 1_int8, 1_int8/)), &
            assert_neq((/2_int8, 1_int8, 1_int8/), (/1_int8, 1_int8, 1_int8/)), &
            assert_neq((/2_int8, 1_int8, 1_int8/), (/1_int8, 1_int8/)), &
            assert_eq((/1_int16, 1_int16, 1_int16/), (/1_int16, 1_int16, 1_int16/)), &
            assert_neq((/2_int16, 1_int16, 1_int16/), (/1_int16, 1_int16, 1_int16/)), &
            assert_neq((/2_int16, 1_int16, 1_int16/), (/1_int16, 1_int16/)), &
            assert_eq((/1_int32, 1_int32, 1_int32/), (/1_int32, 1_int32, 1_int32/)), &
            assert_neq((/2_int32, 1_int32, 1_int32/), (/1_int32, 1_int32, 1_int32/)), &
            assert_neq((/2_int32, 1_int32, 1_int32/), (/1_int32, 1_int32/)), &
            assert_eq((/1_int64, 1_int64, 1_int64/), (/1_int64, 1_int64, 1_int64/)), &
            assert_neq((/2_int64, 1_int64, 1_int64/), (/1_int64, 1_int64, 1_int64/)), &
            assert_neq((/2_int64, 1_int64, 1_int64/), (/1_int64, 1_int64/)), &
            assert_eq((/.true., .false./), (/.true., .false./)), &
            assert_neq((/.true., .false./), (/.true./)), &
            assert_neq((/.true., .false./), (/.true., .true./)) &
        /), &
        name = "Array tests" &
    )

    tests = (/ logical_tests, integer_tests, real_tests, string_tests, array_tests/)

    failure_tests = new_testset(    &
        (/  &
            assert(.false.), &
            assert_eq(2.0_real64, 3.0_real64), &
            assert_eq(2.0_real128, 3.0_real128), &
            assert_geq(2.0, 3.0), &
            assert_gt(3_int32, 4_int32), &
            assert_approx(2.0_real64, 3.0_real64), &
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

    ! text styling
    print*, style_text("test", bg_color = "black", style = "hidden")
    print*, style_text("test", bg_color = "white", style = "underline")
    print*, style_text("test", bg_color = "green", style = "strike")
    print*, style_text("test", bg_color = "light yellow", style = "italic")
    print*, style_text("test", bg_color = "gray", style = "dim")
    print*, style_text("test", bg_color = "light green", style = "blink")
    print*, style_text("test", bg_color = "light pink", style = "fast blink")
    print*, style_text("test", bg_color = "yellow", style = "invert")
    print*, style_text("test", bg_color = "blue", style = "circle")
    print*, style_text("test", bg_color = "aqua", style = "frame")
    print*, style_text("test", bg_color = "purple", style = "overline")
    print*, style_text("test", bg_color = "white", style = "overline")
    print*, style_text("test", bg_color = "pink", style = "bold")
    print*, style_text("test", bg_color = "peach", style = "bold")
    print*, style_text("test", bg_color = "light blue", style = "bold")
    print*, get_color_code("blue", "none")

    write(*, *) NEW_LINE('a')//"Running actual tests..."
    call run_and_exit(tests)
end program
