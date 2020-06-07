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
!!       * Array
!!       * Real128
!!       * Complex
!!       * int8, int16
module fort_test

    use iso_fortran_env

    implicit none 

    private 

    public::    TestSet, Result, new_testset, print_and_exit, assert_eq, assert_neq, assert_positive, &
                assert_negative, assert_gt, assert_geq, assert_lt, assert_leq, assert_approx
    
    type Result
        character(len = :), allocatable:: assertion
        logical:: passed
    end type

    type TestSet
        character(len = :), allocatable:: name
        type(Result), dimension(:), allocatable:: test_list
        integer:: num_passed, num_failed
    end type


    !> Test whether two things are equal
    !! @param[in] arg1
    !! @param[out] arg2 
    !! @return Result
    interface assert_eq
        procedure int32_assert_eq, int64_assert_eq, &
                  real32_assert_eq, real64_assert_eq, &
                  logical_assert_eq, string_assert_eq
    end interface

    interface assert_neq
        procedure   int32_assert_neq, int64_assert_neq, &
                    real32_assert_neq, real64_assert_neq, &
                    logical_assert_neq, string_assert_neq
    end interface

    interface assert_positive
        procedure   int32_assert_positive, int64_assert_positive, &
                    real32_assert_positive, real64_assert_positive
    end interface

    interface assert_negative
        procedure   int32_assert_negative, int64_assert_negative, &
                    real32_assert_negative, real64_assert_negative
    end interface

    interface assert_geq
        procedure   int32_assert_geq, int64_assert_geq, &
                    real32_assert_geq, real64_assert_geq
    end interface 

    interface assert_leq
        procedure   int32_assert_leq, int64_assert_leq, &
                    real32_assert_leq, real64_assert_leq
    end interface 

    interface assert_gt
        procedure   int32_assert_gt, int64_assert_gt, &
                    real32_assert_gt, real64_assert_gt
    end interface

    interface assert_lt
        procedure   int32_assert_lt, int64_assert_lt, &
                    real32_assert_lt, real64_assert_lt
    end interface

    interface assert_approx
        procedure real32_assert_approx, real64_assert_approx
    end interface assert_approx

    contains
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

        function logical_assert_eq(arg1, arg2) result(my_result)
            logical, intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 5):: arg1_str, arg2_str

            arg1_str = merge("True ", "False", arg1)
            arg2_str = merge("True ", "False", arg2)

            my_result = build_assertion(arg1_str, arg2_str, (arg1 .eqv. arg2), "==")
        end function 

        function int32_assert_eq(arg1, arg2) result(my_result)
            integer (int32), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(I4)') arg1
            write(arg2_str, '(I4)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 == arg2), "==")
        end function

        function int64_assert_eq(arg1, arg2) result(my_result)
            integer (int64), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(I8)') arg1
            write(arg2_str, '(I8)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 == arg2), "==")
        end function

        function real32_assert_eq(arg1, arg2) result(my_result)
            real, intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(f20.14)') arg1
            write(arg2_str, '(f20.14)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 == arg2), "==")
        end 

        function real64_assert_eq(arg1, arg2) result(my_result)
            real(real64), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(d31.17)') arg1
            write(arg2_str, '(d31.17)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 == arg2), "==")
        end function

        function string_assert_eq(arg1, arg2) result(my_result)
            character(len = *), intent(in):: arg1, arg2
            type(Result):: my_result

            my_result = build_assertion(arg1, arg2, arg1 == arg2, "==")
        end function

        function logical_assert_neq(arg1, arg2) result(my_result)
            logical, intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 5):: arg1_str, arg2_str

            arg1_str = merge("True ", "False", arg1)
            arg2_str = merge("True ", "False", arg2)

            my_result = build_assertion(arg1_str, arg2_str, (arg1 .neqv. arg2), "!=")
        end function

        function int32_assert_neq(arg1, arg2) result(my_result)
            integer(int32), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(I4)') arg1
            write(arg2_str, '(I4)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 .ne. arg2), "!=")
        end function

        function int64_assert_neq(arg1, arg2) result(my_result)
            integer(int64), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(I8)') arg1
            write(arg2_str, '(I8)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 .ne. arg2), "!=")
        end function

        function real32_assert_neq(arg1, arg2) result(my_result)
            real, intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(f20.14)') arg1
            write(arg2_str, '(f20.14)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 .ne. arg2), "!=")
        end function 

        function real64_assert_neq(arg1, arg2) result(my_result)
            real(real64), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(d31.17)') arg1
            write(arg2_str, '(d31.17)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 .ne. arg2), "!=")
        end function 

        function string_assert_neq(arg1, arg2) result(my_result)
            character(len = *), intent(in):: arg1, arg2
            type(Result):: my_result

            my_result = build_assertion(arg1, arg2, arg1 .ne. arg2, "!=")
        end function

        function int32_assert_positive(arg1) result(my_result)
            integer(int32), intent(in):: arg1
            type(Result):: my_result
            character(len = 32):: arg1_str

            write(arg1_str, '(I4)') arg1
            
            my_result%assertion = trim(adjustl(arg1_str))//" is positive"
            my_result%passed = (arg1 > 0)
        end function
        
        function int64_assert_positive(arg1) result(my_result)
            integer(int64), intent(in):: arg1
            type(Result):: my_result
            character(len = 32):: arg1_str

            write(arg1_str, '(I8)') arg1
            
            my_result%assertion = trim(adjustl(arg1_str))//" is positive"
            my_result%passed = (arg1 > 0)
        end function

        function real32_assert_positive(arg1) result(my_result)
            real(real32), intent(in):: arg1
            type(Result):: my_result
            character(len = 32):: arg1_str

            write(arg1_str, '(f20.14)') arg1
            
            my_result%assertion = trim(adjustl(arg1_str))//" is positive"
            my_result%passed = (arg1 > 0)
        end function
        
        function real64_assert_positive(arg1) result(my_result)
            real(real64), intent(in):: arg1
            type(Result):: my_result
            character(len = 32):: arg1_str

            write(arg1_str, '(d31.17)') arg1
            
            my_result%assertion = trim(adjustl(arg1_str))//" is positive"
            my_result%passed = (arg1 > 0)
        end function

        function int32_assert_negative(arg1) result(my_result)
            integer(int32), intent(in):: arg1
            type(Result):: my_result
            character(len = 32):: arg1_str

            write(arg1_str, '(I4)') arg1
            
            my_result%assertion = trim(adjustl(arg1_str))//" is negative"
            my_result%passed = (arg1 < 0)
        end function

        function int64_assert_negative(arg1) result(my_result)
            integer(int64), intent(in):: arg1
            type(Result):: my_result
            character(len = 32):: arg1_str

            write(arg1_str, '(I8)') arg1
            
            my_result%assertion = trim(adjustl(arg1_str))//" is negative"
            my_result%passed = (arg1 < 0)
        end function

        function real32_assert_negative(arg1) result(my_result)
            real(real32), intent(in):: arg1
            type(Result):: my_result
            character(len = 32):: arg1_str

            write(arg1_str, '(f20.14)') arg1
            
            my_result%assertion = trim(adjustl(arg1_str))//" is negative"
            my_result%passed = (arg1 < 0)
        end function
        
        function real64_assert_negative(arg1) result(my_result)
            real(real64), intent(in):: arg1
            type(Result):: my_result
            character(len = 32):: arg1_str

            write(arg1_str, '(d31.17)') arg1
            
            my_result%assertion = trim(adjustl(arg1_str))//" is negative"
            my_result%passed = (arg1 < 0)
        end function

        function int32_assert_gt(arg1, arg2) result(my_result)
            integer(int32), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(I4)') arg1
            write(arg2_str, '(I4)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 > arg2), ">")
        end function

        function int64_assert_gt(arg1, arg2) result(my_result)
            integer(int64), intent(in):: arg1, arg2
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

            write(arg1_str, '(f20.14)') arg1
            write(arg2_str, '(f20.14)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 > arg2), ">")
        end function 

        function real64_assert_gt(arg1, arg2) result(my_result)
            real(real64), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(d31.17)') arg1
            write(arg2_str, '(d31.17)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 > arg2), ">")
        end function

        function int32_assert_geq(arg1, arg2) result(my_result)
            integer(int32), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(I4)') arg1
            write(arg2_str, '(I4)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 >= arg2), ">=")
        end function

        function int64_assert_geq(arg1, arg2) result(my_result)
            integer(int64), intent(in):: arg1, arg2
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

            write(arg1_str, '(f20.14)') arg1
            write(arg2_str, '(f20.14)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 >= arg2), ">=")
        end function 

        function real64_assert_geq(arg1, arg2) result(my_result)
            real(real64), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(d31.17)') arg1
            write(arg2_str, '(d31.17)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 >= arg2), ">=")
        end function

        function int32_assert_lt(arg1, arg2) result(my_result)
            integer(int32), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(I4)') arg1
            write(arg2_str, '(I4)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 < arg2), "<")
        end function

        function int64_assert_lt(arg1, arg2) result(my_result)
            integer(int64), intent(in):: arg1, arg2
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

            write(arg1_str, '(f20.14)') arg1
            write(arg2_str, '(f20.14)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 < arg2), "<")
        end function 

        function real64_assert_lt(arg1, arg2) result(my_result)
            real(real64), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(d31.17)') arg1
            write(arg2_str, '(d31.17)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 < arg2), "<")
        end function

        function int32_assert_leq(arg1, arg2) result(my_result)
            integer(int32), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(I4)') arg1
            write(arg2_str, '(I4)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 <= arg2), "<=")
        end function

        function int64_assert_leq(arg1, arg2) result(my_result)
            integer(int64), intent(in):: arg1, arg2
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

            write(arg1_str, '(f20.14)') arg1
            write(arg2_str, '(f20.14)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 <= arg2), "<=")
        end function 

        function real64_assert_leq(arg1, arg2) result(my_result)
            real(real64), intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(d31.17)') arg1
            write(arg2_str, '(d31.17)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 <= arg2), "<=")
        end function
        
        function real32_assert_approx(arg1, arg2, rtol, atol) result(my_result)
            real, intent(in):: arg1, arg2
            real, optional:: rtol, atol
            real:: relative_tolerance, absolute_tolerance
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str
            logical:: passed

            relative_tolerance = sqrt(EPSILON(arg1))
            absolute_tolerance = 0.0d0

            if (present(rtol)) relative_tolerance = rtol
   
            if (present(atol)) absolute_tolerance = atol

            write(arg1_str, '(f20.14)') arg1
            write(arg2_str, '(f20.14)') arg2

            passed = (abs(arg1 - arg2) .le. (absolute_tolerance + relative_tolerance*max(abs(arg1), abs(arg2))))

            my_result = build_assertion(arg1_str, arg2_str, passed, "~=")
        end function real32_assert_approx

        function real64_assert_approx(arg1, arg2, rtol, atol) result(my_result)
            real(real64), intent(in):: arg1, arg2
            real(real64), optional:: rtol, atol
            real(real64):: relative_tolerance, absolute_tolerance
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str
            logical:: passed

            relative_tolerance = sqrt(EPSILON(arg1))
            absolute_tolerance = 0.0d0

            if (present(rtol)) relative_tolerance = rtol
   
            if (present(atol)) absolute_tolerance = atol

            write(arg1_str, '(d31.17)') arg1
            write(arg2_str, '(d31.17)') arg2

            passed = (abs(arg1 - arg2) .le. (absolute_tolerance + relative_tolerance * max(abs(arg1), abs(arg2))))

            my_result = build_assertion(arg1_str, arg2_str, passed, "~=")
        end function real64_assert_approx
        
        function build_assertion(arg1_str, arg2_str, passed, comparision) result(my_result)
            character(len = *):: arg1_str, arg2_str, comparision
            type(Result):: my_result
            logical:: passed

            my_result%assertion = trim(adjustl(arg1_str))//" "//comparision//" "//trim(adjustl(arg2_str))
            my_result%passed = passed
        end function build_assertion

        subroutine print_result_msg(my_result, test_number)
            type(Result), intent(in)::my_result
            integer, intent(in):: test_number
            character(len = :), allocatable:: output_string, test_name_string
            character(len = 2):: test_number_string

            write(test_number_string, '(I2)') test_number
            test_name_string = '   Test '//test_number_string

            if (.not. my_result%passed) then
!                output_string = test_name_string//" passed."
!            else
                output_string = achar(27)//'[31m'//test_name_string//' failed.'//achar(27)//'[0m'//NEW_LINE('A')// &
                                "       "//'      Assertion "'//my_result%assertion//'" not satisfied'
                write(*,*) output_string
            endif
        end subroutine print_result_msg

        subroutine print_testset_results(my_testset, testset_number)
            character(len = 8):: num_passed_string, num_failed_string, total_string, number_string
            character(len = 20):: name_string
            type(TestSet), intent(in) :: my_testset
            integer, intent(in):: testset_number
            integer:: i, num_tests = 0

            write(num_passed_string, '(I2)') my_testset%num_passed
            write(num_failed_string, '(I2)') my_testset%num_failed
            write(total_string, '(I2)') my_testset%num_passed + my_testset%num_failed
            write(number_string, '(I2)') testset_number

            if (my_testset%name == 'noname') then
                name_string = "Test Set "//trim(adjustl(number_string)) 
            else
                name_string = my_testset%name
            endif 

            if (my_testset%num_failed > 0) then
                write(*, *) name_string//'|'//& 
                     achar(27)//'[92m'//adjustr(num_passed_string)//achar(27)//'[0m'//&
                     achar(27)//'[31m'//adjustr(num_failed_string)//achar(27)//'[0m'//&
                     achar(27)//'[96m'//adjustr(total_string)//achar(27)//'[0m'
                num_tests = size(my_testset%test_list)
                do i = 1, num_tests
                    call print_result_msg(my_testset%test_list(i), i)
                end do 
            else
                write(*, *) name_string//'|'//& 
                     achar(27)//'[92m'//adjustr(num_passed_string)//achar(27)//'[0m'//&
                     "        "// & 
                     achar(27)//'[96m'//adjustr(total_string)//achar(27)//'[0m'
            endif

        end subroutine print_testset_results

        subroutine print_and_exit(testsets)
            integer:: i, status = 0
            type(TestSet), dimension(:), intent(in):: testsets
            write(*, *) achar(27)//'[1m'//'Test summary:       '//achar(27)//'[0m|'//& 
                     achar(27)//'[1m'//achar(27)//'[92m'//'  Passed'//achar(27)//'[0m'//achar(27)//'[0m'//&
                     achar(27)//'[1m'//achar(27)//'[31m'//'  Failed'//achar(27)//'[0m'//achar(27)//'[0m'//&
                     achar(27)//'[1m'//achar(27)//'[96m'//'   Total'//achar(27)//'[0m'//achar(27)//'[0m'

            do i = 1, size(testsets)
                call print_testset_results(testsets(i), i)
                if ((testsets(i)%num_failed > 0) .and. (status == 0)) then
                    status = 1
                endif
            end do

            call exit(status)
        end subroutine
end module 