!> The fort_test module
!!
!! Part of the fort_test repository https://github.com/archermarx/fort_test
!! @author Thomas Marks
!!
!! Published with the GPL license
!! @todo
!!  * More documentation
!!  * Simpler test set declaration?
!!  * Allow running of tests to be deferred/ignored
!!  * Look into preprocessor macros to print lines of source code
!!  * Make little example project based on tutorial
!!  * More types
!!       * Real128
!!       * Complex
!!       * int8, int16
module fort_test

    use, intrinsic:: iso_fortran_env, only : &
        stderr => error_unit, &
        stdin => input_unit, &
        stdout => output_unit, &
        f32  => real32, &
        f64  => real64, &
        f128 => real128, &
        i8  => int8, &
        i16 => int16, &
        i32 => int32, &
        i64 => int64

    implicit none

    private

    public::    TestSet, Result, new_testset, run_all, assert_eq, assert_neq, &
                assert_gt, assert_geq, assert_lt, assert_leq, assert_approx, assert, &
                run_and_exit

    type Result
        character(len = :), allocatable:: assertion
        logical:: passed
    end type

    type TestSet
        character(len = :), allocatable:: name
        type(Result), dimension(:), allocatable:: test_list
        integer:: num_passed, num_failed
    end type

    character(len = *), parameter:: FG_COLOR_SUMMARY = "light white"
    character(len = *), parameter:: FG_COLOR_PASS = "light green"
    character(len = *), parameter:: FG_COLOR_FAIL = "red"
    character(len = *), parameter:: FG_COLOR_TOTAL = "cyan"

    character(len = *), parameter:: BG_COLOR_HEADER = "black"
    character(len = *), parameter:: HEADER_SUMMARY = "Test summary"
    character(len = *), parameter:: HEADER_PASS = "Passed"
    character(len = *), parameter:: HEADER_FAIL = " Failed"
    character(len = *), parameter:: HEADER_TOTAL = " Total"

    character(len = *), parameter:: FLOAT_FORMAT = '(g0)'

    !> Convert the argument to a string
    !! @param[in] arg1
    !! @param[in] arg2
    !! @return Result
    interface to_string
        module procedure logical_to_string, logical_arr_to_string
        module procedure int8_to_string, int8_arr_to_string
        module procedure int16_to_string, int16_arr_to_string
        module procedure int32_to_string, int32_arr_to_string
        module procedure int64_to_string, int64_arr_to_string
        module procedure real32_to_string, real32_arr_to_string
        module procedure real64_to_string, real64_arr_to_string
        module procedure real128_to_string, real128_arr_to_string
    end interface

    !> Test whether two things are equal
    !! @param[in] arg1
    !! @param[in] arg2
    !! @return Result
    interface assert_eq
        module procedure int8_assert_eq, int8_arr_assert_eq
        module procedure int16_assert_eq, int16_arr_assert_eq
        module procedure int32_assert_eq, int32_arr_assert_eq
        module procedure int64_assert_eq, int64_arr_assert_eq
        module procedure real32_assert_eq, real32_arr_assert_eq
        module procedure real64_assert_eq, real64_arr_assert_eq
        module procedure real128_assert_eq, real128_arr_assert_eq
        module procedure logical_assert_eq, logical_arr_assert_eq
        module procedure string_assert_eq
    end interface

    !> Test whether two things are  not equal
    !! @param[in] arg1
    !! @param[in] arg2
    !! @return Result
    interface assert_neq
        module procedure int8_assert_neq, int8_arr_assert_neq
        module procedure int16_assert_neq, int16_arr_assert_neq
        module procedure int32_assert_neq, int32_arr_assert_neq
        module procedure int64_assert_neq, int64_arr_assert_neq
        module procedure real32_assert_neq, real32_arr_assert_neq
        module procedure real64_assert_neq, real64_arr_assert_neq
        module procedure real128_assert_neq, real128_arr_assert_neq
        module procedure logical_assert_neq, logical_arr_assert_neq
        module procedure string_assert_neq
    end interface

    !> Test whether arg1 >= arg2
    !! @param[in] arg1
    !! @param[in] arg2
    !! @return Result
    interface assert_geq
        procedure   int8_assert_geq, int16_assert_geq, &
                    int32_assert_geq, int64_assert_geq, &
                    real32_assert_geq, real64_assert_geq, real128_assert_geq
    end interface

    !> Test whether arg1 <= arg2
    !! @param[in] arg1
    !! @param[in] arg2
    !! @return Result
    interface assert_leq
        procedure   int8_assert_leq, int16_assert_leq, &
                    int32_assert_leq, int64_assert_leq, &
                    real32_assert_leq, real64_assert_leq, real128_assert_leq
    end interface

    !> Test whether arg1 > arg2
    !! @param[in] arg1
    !! @param[in] arg2
    !! @return Result
    interface assert_gt
        procedure   int8_assert_gt, int16_assert_gt, &
                    int32_assert_gt, int64_assert_gt, &
                    real32_assert_gt, real64_assert_gt, real128_assert_gt
    end interface

    !> Test whether arg1 < arg2
    !! @param[in] arg1
    !! @param[in] arg2
    !! @return Result
    interface assert_lt
        procedure   int8_assert_lt, int16_assert_lt, &
                    int32_assert_lt, int64_assert_lt, &
                    real32_assert_lt, real64_assert_lt, real128_assert_lt
    end interface

    !> Test whether arg1 ~= arg2
    !! @param[in] arg1
    !! @param[in] arg2
    !! @param[in] atol
    !! @param[in] rol
    !! @return Result
    interface assert_approx
        procedure real32_assert_approx, real64_assert_approx, real128_assert_approx
    end interface assert_approx

    contains

    !============================================================
    !               testset construction
    !============================================================

    function new_testset(test_list, name) result(my_testset)
        type(TestSet):: my_testset
        character(len = *), optional:: name
        character(len = :), allocatable::testset_name
        type(Result), dimension(:), intent(in):: test_list
        integer:: i, num_tests

        if (present(name)) then
            testset_name = name
        else
            testset_name = "noname"
        endif

        my_testset%name = testset_name
        my_testset%test_list = test_list
        my_testset%num_passed = 0
        my_testset%num_failed = 0

        num_tests = size(my_testset%test_list)

        ! TODO: Move this into testset constructor
        do i = 1, num_tests
            if (my_testset%test_list(i)%passed) then
                my_testset%num_passed = my_testset%num_passed + 1
            else
                my_testset%num_failed = my_testset%num_failed + 1
            endif
        end do
    end function

    function assert(bool) result(my_result)
        logical, intent(in):: bool
        type(Result):: my_result

        if (bool) then
            my_result%assertion = "true"
        else
            my_result%assertion = "false"
        endif

        my_result%passed = bool
    end function

    function build_assertion(arg1_str, arg2_str, passed, comparision) result(my_result)
        character(len = *):: arg1_str, arg2_str, comparision
        type(Result):: my_result
        logical:: passed

        my_result%assertion = trim(adjustl(arg1_str))//" "//comparision//" "//trim(adjustl(arg2_str))
        my_result%passed = passed
    end function build_assertion

    !============================================================
    !               methods for to_string
    !============================================================
    pure function logical_to_string(val) result(str)
        logical, intent(in):: val
        character(len = :), allocatable:: str
        if (val) then
            str = "true"
        else
            str = "false"
        endif
    end function

    pure function logical_arr_to_string(arr) result(str)
        logical, intent(in):: arr(:)
        character(len = :), allocatable:: str
        integer(i32):: i
        str = "["
        do i = 1, size(arr)
            str = str // trim(adjustl(to_string(arr(i))))
            if (i < size(arr)) str = str // ", "
        end do
        str = str // "]"
    end function

    pure function int8_to_string(val) result(str)
        integer(i8), intent(in):: val
        character(len = 20):: str_intermediate
        character(len = :), allocatable:: str
        write(str_intermediate, '(g0)') val
        str = trim(adjustl(str_intermediate))
    end function

    pure function int8_arr_to_string(arr) result(str)
        integer(i8), intent(in):: arr(:)
        character(len = :), allocatable:: str
        integer(i32):: i
        str = "["
        do i = 1, size(arr)
            str = str // trim(adjustl(to_string(arr(i))))
            if (i < size(arr)) str = str // ", "
        end do
        str = str // "]"
    end function

    pure function int16_to_string(val) result(str)
        integer(i16), intent(in):: val
        character(len = 20):: str_intermediate
        character(len = :), allocatable:: str
        write(str_intermediate, '(g0)') val
        str = trim(adjustl(str_intermediate))
    end function

    pure function int16_arr_to_string(arr) result(str)
        integer(i16), intent(in):: arr(:)
        character(len = :), allocatable:: str
        integer(i32):: i
        str = "["
        do i = 1, size(arr)
            str = str // trim(adjustl(to_string(arr(i))))
            if (i < size(arr)) str = str // ", "
        end do
        str = str // "]"
    end function

    pure function int32_to_string(val) result(str)
        integer(i32), intent(in):: val
        character(len = 20):: str_intermediate
        character(len = :), allocatable:: str
        write(str_intermediate, '(g0)') val
        str = trim(adjustl(str_intermediate))
    end function

    pure function int32_arr_to_string(arr) result(str)
        integer(i32), intent(in):: arr(:)
        character(len = :), allocatable:: str
        integer(i32):: i
        str = "["
        do i = 1, size(arr)
            str = str // trim(adjustl(to_string(arr(i))))
            if (i < size(arr)) str = str // ", "
        end do
        str = str // "]"
    end function

    pure function int64_to_string(val) result(str)
        integer(i64), intent(in):: val
        character(len = 20):: str_intermediate
        character(len = :), allocatable:: str
        write(str_intermediate, '(g0)') val
        str = trim(adjustl(str_intermediate))
    end function

    pure function int64_arr_to_string(arr) result(str)
        integer(i64), intent(in):: arr(:)
        character(len = :), allocatable:: str
        integer(i32):: i
        str = "["
        do i = 1, size(arr)
            str = str // trim(adjustl(to_string(arr(i))))
            if (i < size(arr)) str = str // ", "
        end do
        str = str // "]"
    end function

    pure function real32_to_string(val) result(str)
        real(f32), intent(in):: val
        character(len = 50):: str_intermediate
        character(len = :), allocatable:: str
        write(str_intermediate, FLOAT_FORMAT) val
        str = trim(adjustl(str_intermediate))
    end function

    pure function real32_arr_to_string(arr) result(str)
        real(f32), intent(in):: arr(:)
        character(len = :), allocatable:: str
        integer(i32):: i
        str = "["
        do i = 1, size(arr)
            str = str // trim(adjustl(to_string(arr(i))))
            if (i < size(arr)) str = str // ", "
        end do
        str = str // "]"
    end function

    pure function real64_to_string(val) result(str)
        real(f64), intent(in):: val
        character(len = 50):: str_intermediate
        character(len = :), allocatable:: str
        write(str_intermediate, FLOAT_FORMAT) val
        str = trim(adjustl(str_intermediate))
    end function

    pure function real64_arr_to_string(arr) result(str)
        real(f64), intent(in):: arr(:)
        character(len = :), allocatable:: str
        integer(i32):: i
        str = "["
        do i = 1, size(arr)
            str = str // trim(adjustl(to_string(arr(i))))
            if (i < size(arr)) str = str // ", "
        end do
        str = str // "]"
    end function

    pure function real128_to_string(val) result(str)
        real(f128), intent(in):: val
        character(len = 64):: str_intermediate
        character(len = :), allocatable:: str
        write(str_intermediate, FLOAT_FORMAT) val
        str = trim(adjustl(str_intermediate))
    end function

    pure function real128_arr_to_string(arr) result(str)
        real(f128), intent(in):: arr(:)
        character(len = :), allocatable:: str
        integer(i32):: i
        str = "["
        do i = 1, size(arr)
            str = str // trim(adjustl(to_string(arr(i))))
            if (i < size(arr)) str = str // ", "
        end do
        str = str // "]"
    end function

    !============================================================
    !               methods for assert_eq
    !============================================================
    type(Result) function logical_assert_eq(arg1, arg2) result(my_result)
        logical, intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 .eqv. arg2), "==")
    end function

    type(Result) function logical_arr_assert_eq(arg1, arg2) result(my_result)
        logical, intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) == size(arg2) .and. all(arg1 .eqv. arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "==")
    end function

    type(Result) function int8_assert_eq(arg1, arg2) result(my_result)
        integer(i8), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 == arg2), "==")
    end function

    type(Result) function int8_arr_assert_eq(arg1, arg2) result(my_result)
        integer(i8), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) == size(arg2) .and. all(arg1 == arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "==")
    end function

    type(Result) function int16_assert_eq(arg1, arg2) result(my_result)
        integer(i16), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 == arg2), "==")
    end function

    type(Result) function int16_arr_assert_eq(arg1, arg2) result(my_result)
        integer(i16), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) == size(arg2) .and. all(arg1 == arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "==")
    end function

    type(Result) function int32_assert_eq(arg1, arg2) result(my_result)
        integer(i32), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 == arg2), "==")
    end function

    type(Result) function int32_arr_assert_eq(arg1, arg2) result(my_result)
        integer(i32), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) == size(arg2) .and. all(arg1 == arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "==")
    end function

    type(Result) function int64_assert_eq(arg1, arg2) result(my_result)
        integer(i64), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 == arg2), "==")
    end function

    type(Result) function int64_arr_assert_eq(arg1, arg2) result(my_result)
        integer(i64), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) == size(arg2) .and. all(arg1 == arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "==")
    end function

    type(Result) function real32_assert_eq(arg1, arg2) result(my_result)
        real(f32), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 == arg2), "==")
    end function

    type(Result) function real32_arr_assert_eq(arg1, arg2) result(my_result)
        real(f32), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) == size(arg2) .and. all(arg1 == arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "==")
    end function

    type(Result) function real64_assert_eq(arg1, arg2) result(my_result)
        real(f64), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 == arg2), "==")
    end function

    type(Result) function real64_arr_assert_eq(arg1, arg2) result(my_result)
        real(f64), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) == size(arg2) .and. all(arg1 == arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "==")
    end function

    type(Result) function real128_assert_eq(arg1, arg2) result(my_result)
        real(f128), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 == arg2), "==")
    end function

    type(Result) function real128_arr_assert_eq(arg1, arg2) result(my_result)
        real(f128), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) == size(arg2) .and. all(arg1 == arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "==")
    end function

    type(Result) function string_assert_eq(arg1, arg2) result(my_result)
        character(len = *), intent(in):: arg1, arg2
        my_result = build_assertion(arg1, arg2, arg1 == arg2, "==")
    end function

    !============================================================
    !               methods for assert_neq
    !============================================================

    type(Result) function logical_assert_neq(arg1, arg2) result(my_result)
        logical, intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 .neqv. arg2), "!=")
    end function

    type(Result) function logical_arr_assert_neq(arg1, arg2) result(my_result)
        logical, intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) /= size(arg2) .or. any(arg1 .neqv. arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "!=")
    end function

    type(Result) function int8_assert_neq(arg1, arg2) result(my_result)
        integer(i8), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 /= arg2), "!=")
    end function

    type(Result) function int8_arr_assert_neq(arg1, arg2) result(my_result)
        integer(i8), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) /= size(arg2) .or.any(arg1 /= arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "!=")
    end function

    type(Result) function int16_assert_neq(arg1, arg2) result(my_result)
        integer(i16), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 /= arg2), "!=")
    end function

    type(Result) function int16_arr_assert_neq(arg1, arg2) result(my_result)
        integer(i16), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) /= size(arg2) .or.any(arg1 /= arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "!=")
    end function

    type(Result) function int32_assert_neq(arg1, arg2) result(my_result)
        integer(i32), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 /= arg2), "!=")
    end function

    type(Result) function int32_arr_assert_neq(arg1, arg2) result(my_result)
        integer(i32), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) /= size(arg2) .or.any(arg1 /= arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "!=")
    end function

    type(Result) function int64_assert_neq(arg1, arg2) result(my_result)
        integer(i64), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 /= arg2), "!=")
    end function

    type(Result) function int64_arr_assert_neq(arg1, arg2) result(my_result)
        integer(i64), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) /= size(arg2) .or. any(arg1 /= arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "!=")
    end function

    type(Result) function real32_assert_neq(arg1, arg2) result(my_result)
        real(f32), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 /= arg2), "!=")
    end function

    type(Result) function real32_arr_assert_neq(arg1, arg2) result(my_result)
        real(f32), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) /= size(arg2) .or. any(arg1 /= arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "!=")
    end function

    type(Result) function real64_assert_neq(arg1, arg2) result(my_result)
        real(f64), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 /= arg2), "!=")
    end function

    type(Result) function real64_arr_assert_neq(arg1, arg2) result(my_result)
        real(f64), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) /= size(arg2) .or. any(arg1 /= arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "!=")
    end function

    type(Result) function real128_assert_neq(arg1, arg2) result(my_result)
        real(f128), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 /= arg2), "!=")
    end function

    type(Result) function real128_arr_assert_neq(arg1, arg2) result(my_result)
        real(f128), intent(in):: arg1(:), arg2(:)
        logical:: assertion_result
        assertion_result = size(arg1) /= size(arg2) .or. any(arg1 /= arg2)
        my_result = build_assertion(to_string(arg1), to_string(arg2), assertion_result, "!=")
    end function

    type(Result) function string_assert_neq(arg1, arg2) result(my_result)
        character(len = *), intent(in):: arg1, arg2
        my_result = build_assertion(arg1, arg2, arg1 /= arg2, "!=")
    end function

    !============================================================
    !               methods for assert_gt
    !============================================================

    function int8_assert_gt(arg1, arg2) result(my_result)
        integer(i8), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 > arg2), ">")
    end function

    function int16_assert_gt(arg1, arg2) result(my_result)
        integer(i16), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 > arg2), ">")
    end function

    function int32_assert_gt(arg1, arg2) result(my_result)
        integer(i32), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 > arg2), ">")
    end function

    function int64_assert_gt(arg1, arg2) result(my_result)
        integer(i64), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I8)') arg1
        write(arg2_str, '(I8)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 > arg2), ">")
    end function

    function real32_assert_gt(arg1, arg2) result(my_result)
        real, intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 > arg2), ">")
    end function

    type(Result) function real64_assert_gt(arg1, arg2) result(my_result)
        real(f64), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 > arg2), ">")
    end function

    type(Result) function real128_assert_gt(arg1, arg2) result(my_result)
        real(f128), intent(in):: arg1, arg2
        my_result = build_assertion(to_string(arg1), to_string(arg2), (arg1 > arg2), ">")
    end function

    !============================================================
    !               methods for assert_geq
    !============================================================

    function int8_assert_geq(arg1, arg2) result(my_result)
        integer(i8), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 >= arg2), ">=")
    end function

    function int16_assert_geq(arg1, arg2) result(my_result)
        integer(i16), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I8)') arg1
        write(arg2_str, '(I8)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 >= arg2), ">=")
    end function

    function int32_assert_geq(arg1, arg2) result(my_result)
        integer(i32), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 >= arg2), ">=")
    end function

    function int64_assert_geq(arg1, arg2) result(my_result)
        integer(i64), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I8)') arg1
        write(arg2_str, '(I8)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 >= arg2), ">=")
    end function

    function real32_assert_geq(arg1, arg2) result(my_result)
        real, intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 >= arg2), ">=")
    end function

    function real64_assert_geq(arg1, arg2) result(my_result)
        real(f64), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 >= arg2), ">=")
    end function

    function real128_assert_geq(arg1, arg2) result(my_result)
        real(f128), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 128):: arg1_str, arg2_str

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 >= arg2), ">=")
    end function

    !============================================================
    !               methods for assert_lt
    !============================================================

    function int8_assert_lt(arg1, arg2) result(my_result)
        integer(i8), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 < arg2), "<")
    end function

    function int16_assert_lt(arg1, arg2) result(my_result)
        integer(i16), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 < arg2), "<")
    end function

    function int32_assert_lt(arg1, arg2) result(my_result)
        integer(i32), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 < arg2), "<")
    end function

    function int64_assert_lt(arg1, arg2) result(my_result)
        integer(i64), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 < arg2), "<")
    end function

    function real32_assert_lt(arg1, arg2) result(my_result)
        real, intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 < arg2), "<")
    end function

    function real64_assert_lt(arg1, arg2) result(my_result)
        real(f64), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 < arg2), "<")
    end function

    function real128_assert_lt(arg1, arg2) result(my_result)
        real(f128), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 128):: arg1_str, arg2_str

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 < arg2), "<")
    end function

    !============================================================
    !               methods for assert_leq
    !============================================================

    function int8_assert_leq(arg1, arg2) result(my_result)
        integer(i8), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 <= arg2), "<=")
    end function

    function int16_assert_leq(arg1, arg2) result(my_result)
        integer(i16), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 <= arg2), "<=")
    end function

    function int32_assert_leq(arg1, arg2) result(my_result)
        integer(i32), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 <= arg2), "<=")
    end function

    function int64_assert_leq(arg1, arg2) result(my_result)
        integer(i64), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, '(I4)') arg1
        write(arg2_str, '(I4)') arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 <= arg2), "<=")
    end function

    function real32_assert_leq(arg1, arg2) result(my_result)
        real, intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 <= arg2), "<=")
    end function

    function real64_assert_leq(arg1, arg2) result(my_result)
        real(f64), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 <= arg2), "<=")
    end function

    function real128_assert_leq(arg1, arg2) result(my_result)
        real(f128), intent(in):: arg1, arg2
        type(Result):: my_result
        character(len = 64):: arg1_str, arg2_str

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        my_result = build_assertion(arg1_str, arg2_str, (arg1 <= arg2), "<=")
    end function

    !============================================================
    !               methods for assert_approx
    !============================================================

    function real32_assert_approx(arg1, arg2, rtol, atol) result(my_result)
        real(f32), intent(in):: arg1, arg2
        real(f32), optional:: rtol, atol
        real(f32):: relative_tolerance, absolute_tolerance
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str
        logical:: passed

        relative_tolerance = sqrt(EPSILON(arg1))
        absolute_tolerance = 0.0_f32

        if (present(rtol)) relative_tolerance = rtol

        if (present(atol)) absolute_tolerance = atol

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        passed = (abs(arg1 - arg2) <= (absolute_tolerance + relative_tolerance*max(abs(arg1), abs(arg2))))

        my_result = build_assertion(arg1_str, arg2_str, passed, "~=")
    end function

    function real64_assert_approx(arg1, arg2, rtol, atol) result(my_result)
        real(f64), intent(in):: arg1, arg2
        real(f64), optional:: rtol, atol
        real(f64):: relative_tolerance, absolute_tolerance
        type(Result):: my_result
        character(len = 32):: arg1_str, arg2_str
        logical:: passed

        relative_tolerance = sqrt(EPSILON(arg1))
        absolute_tolerance = 0.0_f64

        if (present(rtol)) relative_tolerance = rtol

        if (present(atol)) absolute_tolerance = atol

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        passed = (abs(arg1 - arg2) <= (absolute_tolerance + relative_tolerance * max(abs(arg1), abs(arg2))))

        my_result = build_assertion(arg1_str, arg2_str, passed, "~=")
    end function

    function real128_assert_approx(arg1, arg2, rtol, atol) result(my_result)
        real(f128), intent(in):: arg1, arg2
        real(f128), optional:: rtol, atol
        real(f128):: relative_tolerance, absolute_tolerance
        type(Result):: my_result
        character(len = 64):: arg1_str, arg2_str
        logical:: passed

        relative_tolerance = sqrt(EPSILON(arg1))
        absolute_tolerance = 0.0_f64

        if (present(rtol)) relative_tolerance = rtol

        if (present(atol)) absolute_tolerance = atol

        write(arg1_str, FLOAT_FORMAT) arg1
        write(arg2_str, FLOAT_FORMAT) arg2

        passed = (abs(arg1 - arg2) <= (absolute_tolerance + relative_tolerance * max(abs(arg1), abs(arg2))))

        my_result = build_assertion(arg1_str, arg2_str, passed, "~=")
    end function


    subroutine print_result_msg(my_result, test_number)
        type(Result), intent(in)::my_result
        integer, intent(in):: test_number
        character(len = :), allocatable:: output_string, test_name_string
        character(len = 4):: test_number_string

        write(test_number_string, '(I4)') test_number
        test_name_string = '   Test '//test_number_string

        if (.not. my_result%passed) then
            output_string = achar(27)//'[31m'//test_name_string//' failed.'//achar(27)//'[0m'//NEW_LINE('A')// &
                            "       "//'Assertion "'//my_result%assertion//'" not satisfied'
            write(*,*) output_string
        endif
    end subroutine print_result_msg

    function rpad(str, n) result(padded)
        integer(i32):: n
        character(len = *):: str
        character(len = :), allocatable:: padded

        padded = str
        do while (len(padded) < n)
            padded = padded // " "
        end do
    end function rpad

    function lpad(str, n) result(padded)
        integer(i32):: n
        character(len = *):: str
        character(len = :), allocatable:: padded

        padded = adjustr(rpad(str, n))
    end function lpad

    function num_digits(i) result(n)
        integer(i32):: i, n

        if ( i == 0 ) then
            n = 1
        else
            n = floor(log10(1.0 * i))
        endif

    end function num_digits

    subroutine print_header(column_widths)

        integer(i32):: column_widths(4)
        character(len = :), allocatable:: passed_str, failed_str, total_str
        character(len = :), allocatable:: summary_str

        ! Initialize arrays
        summary_str = rpad(HEADER_SUMMARY, column_widths(1))
        passed_str  = lpad(HEADER_PASS,    column_widths(2))
        failed_str  = lpad(HEADER_FAIL,    column_widths(3))
        total_str   = lpad(HEADER_TOTAL,   column_widths(4))

        ! Write header to stdout
        write(stdout, '(A)') &
            style_text(summary_str, fg_color=FG_COLOR_SUMMARY, bg_color=BG_COLOR_HEADER) // &
            style_text("|",         fg_color=FG_COLOR_SUMMARY, bg_color=BG_COLOR_HEADER) // &
            style_text(passed_str,  fg_color=FG_COLOR_PASS,    bg_color=BG_COLOR_HEADER) // &
            style_text(failed_str,  fg_color=FG_COLOR_FAIL,    bg_color=BG_COLOR_HEADER) // &
            style_text(total_str,   fg_color=FG_COLOR_TOTAL,   bg_color=BG_COLOR_HEADER)

    end subroutine print_header


    subroutine print_testset_results(my_testset, column_widths)
        type(TestSet), intent(in) :: my_testset
        integer(i32):: column_widths(4)

        character(len = column_widths(1)):: name_str
        character(len = column_widths(2)):: num_passed_str
        character(len = column_widths(3)):: num_failed_str
        character(len = column_widths(4)):: num_total_str
        integer(i32):: i, num_tests = 0
        integer(i32):: num_pass, num_fail, num_total

        num_pass = my_testset%num_passed
        num_fail = my_testset%num_failed
        num_total = num_pass + num_fail

        write(num_passed_str, '(I0)') num_pass

        if (num_fail > 0) then
            write(num_failed_str, '(I0)') num_fail
        else
            write(num_failed_str, '(A)')
        endif

        write(num_total_str, '(I0)') num_total

        write(name_str, '(A)') my_testset%name

        write(stdout, '(A)') name_str // '|' // &
                style_text(adjustr(num_passed_str), fg_color = FG_COLOR_PASS)//&
                style_text(adjustr(num_failed_str), fg_color = FG_COLOR_FAIL)//&
                style_text(adjustr(num_total_str), fg_color = FG_COLOR_TOTAL)

        num_tests = size(my_testset%test_list)

        do i = 1, num_tests
            call print_result_msg(my_testset%test_list(i), i)
        end do

    end subroutine print_testset_results

    function run_all(testsets) result(num_failed)
        type(TestSet), dimension(:):: testsets
        integer(i32):: i, num_failed
        integer(i32):: column_widths(4)
        integer(i32):: num_pass, num_fail, num_total
        character(len = 4):: number_string

        ! Initialize column widths o that they can contain the header text
        column_widths(1) = len(HEADER_SUMMARY)
        column_widths(2) = len(HEADER_PASS)
        column_widths(3) = len(HEADER_FAIL)
        column_widths(4) = len(HEADER_TOTAL)

        ! Get testset dimensions for printing
        do i = 1, size(testsets)
            num_pass  = testsets(i)%num_passed
            num_fail  = testsets(i)%num_failed
            num_total = num_pass + num_fail

            ! Give proper name to test set if it doesn't have one
            if (testsets(i)%name == 'noname') then
                write(number_string, '(I4)') i
                testsets(i)%name = "Test Set "// trim(adjustl(number_string))
            end if

            column_widths(1) = max(column_widths(1), len(testsets(i)%name))
            column_widths(2) = max(column_widths(2), num_digits(num_pass))
            column_widths(3) = max(column_widths(3), num_digits(num_fail))
            column_widths(4) = max(column_widths(4), num_digits(num_total))
        end do

        ! Add one to each column width for spacing
        column_widths = column_widths + 1

        ! Print the header
        call print_header(column_widths)

        ! Print results of each testset
        num_failed = 0
        do i = 1, size(testsets)
            call print_testset_results(testsets(i), column_widths)
            num_failed = num_failed + testsets(i)%num_failed
        end do

    end function

    subroutine run_and_exit(testsets)
        type(TestSet), dimension(:), intent(in):: testsets
        integer(i32):: num_failed
        num_failed = run_all(testsets)
        if (num_failed > 0) then
            call exit(1)
        else
            call exit(0)
        endif
    end subroutine

    function get_color_code(color_str, type) result (color_code)

        character(len = *):: color_str
        character(len = 2):: type
        character(len = :), allocatable:: color_code
        character(len = :), allocatable:: c_light, c_dark

        ! Colors
        character(len = *), parameter:: c_black = "0"
        character(len = *), parameter:: c_red = "1"
        character(len = *), parameter:: c_green = "2"
        character(len = *), parameter:: c_yellow = "3"
        character(len = *), parameter:: c_blue = "4"
        character(len = *), parameter:: c_magenta = "5"
        character(len = *), parameter:: c_cyan = "6"
        character(len = *), parameter:: c_white = "7"

        ! Foreground and background colors
        character(len = *), parameter:: c_fg_dark = "3"
        character(len = *), parameter:: c_fg_light = "9"
        character(len = *), parameter:: c_bg_dark = "4"
        character(len = *), parameter:: c_bg_light = "10"

        ! Decide between foreground and background colors
        select case(type)
        case("fg", "foreground")
            c_light = c_fg_light
            c_dark = c_fg_dark
        case("bg", "background")
            c_light = c_bg_light
            c_dark = c_bg_dark
        case default
            c_light = c_fg_light
            c_dark = c_fg_dark
        end select

        ! Assign correct colors based on string
        select case (color_str)
        case("dark grey", "dark gray", "grey", "gray")
            color_code = c_light // c_black
        case("light red", "peach")
            color_code = c_light // c_red
        case("light green")
            color_code = c_light // c_green
        case("light yellow")
            color_code = c_light // c_yellow
        case("light blue")
            color_code = c_light // c_blue
        case("pink", "light magenta")
            color_code = c_light // c_magenta
        case("light aqua", "cyan")
            color_code = c_light // c_cyan
        case("pearl white", "light white")
            color_code = c_light // c_white
        case("black")
            color_code = c_dark // c_black
        case("red")
            color_code = c_dark // c_red
        case("green")
            color_code = c_dark // c_green
        case("yellow")
            color_code = c_dark // c_yellow
        case("blue")
            color_code = c_dark // c_blue
        case("purple", "magenta")
            color_code = c_dark // c_magenta
        case("aqua", "teal", "turquoise")
            color_code = c_dark // c_cyan
        case("white")
            color_code = c_dark // c_white
        case default
            color_code = ""
        end select

    end function get_color_code

    function style_text(str, fg_color, bg_color, style) result(styled)
        character(len = *):: str
        character(len = *), optional:: fg_color, bg_color, style
        character(len = :), allocatable:: fg_color_str, bg_color_str, style_str
        character(len = :), allocatable:: fg_color_code, bg_color_code, style_code
        character(len = :), allocatable:: format_code, styled
        character(len = :), allocatable:: style_fg_sep, fg_bg_sep

        ! Escape characters
        character(len = *), parameter:: c_esc = achar(27)
        character(len = *), parameter:: c_start = c_esc // "["
        character(len = *), parameter:: c_end = "m"
        character(len = *), parameter:: c_clear = c_start // "0" // c_end

        ! Styles
        character(len = *), parameter:: c_bold = "1"
        character(len = *), parameter:: c_dim = "2"
        character(len = *), parameter:: c_ital = "3"
        character(len = *), parameter:: c_undl = "4"
        character(len = *), parameter:: c_blink = "5"
        character(len = *), parameter:: c_blink_fast = "6"
        character(len = *), parameter:: c_invert = "7"
        character(len = *), parameter:: c_hidden = "8"
        character(len = *), parameter:: c_strike = "9"
        character(len = *), parameter:: c_frame = "51"
        character(len = *), parameter:: c_circle = "52"
        character(len = *), parameter:: c_overline = "53"

        if (present(fg_color)) then
            fg_color_str = fg_color
        else
            fg_color_str = "none"
        endif

        if (present(bg_color)) then
            bg_color_str = bg_color
        else
            bg_color_str = "none"
        endif

        if (present(style)) then
            style_str = style
        else
            style_str = "none"
        endif

        select case(style_str)
        case("bold", "bolded")
            style_code = c_bold
        case("underline", "underlined")
            style_code = c_undl
        case("italic", "italicized")
            style_code = c_ital
        case("dim", "dimmed")
            style_code = c_dim
        case("blink", "blinking")
            style_code = c_blink
        case("fast blink", "fast blinking")
            style_code = c_blink_fast
        case("invert", "inverted")
            style_code = c_invert
        case("hidden")
            style_code = c_hidden
        case("strike", "strikethrough")
            style_code = c_strike
        case("circled", "encircled", "circle", "encircle")
            style_code = c_circle
        case("framed", "frame", "boxed", "box")
            style_code = c_frame
        case("overline", "overlined")
            style_code = c_overline
        case default
            style_code = ""
        end select

        fg_color_code = get_color_code(fg_color, "fg")
        bg_color_code = get_color_code(bg_color, "bg")

        if (fg_color_code == "") then
            style_fg_sep = ""
        else
            style_fg_sep = ";"
        end if

        if (bg_color_code == "") then
            fg_bg_sep = ""
        else
            fg_bg_sep = ";"
        end if

        format_code = style_code // style_fg_sep // fg_color_code // fg_bg_sep // bg_color_code

        styled = c_start // format_code // c_end // str // c_clear

    end function

end module fort_test
