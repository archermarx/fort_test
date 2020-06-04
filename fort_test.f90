module fort_test

    type Result
        character(len = :), allocatable:: result_msg
        logical:: passed
    end type

    type Test
        character(len = :), allocatable:: name
        type(Result):: result
    end type

    type TestSet
        character(len = 16):: name
        type(Test), dimension(:), allocatable:: test_list
        integer:: num_passed, num_failed
    end type

    interface
        function testinterface()
            import:: Result
            type(Result)::testinterface
        end function testinterface
    end interface

    interface assert_eq
        procedure z_assert_eq, s_assert_eq, d_assert_eq
    end interface assert_eq

    interface assert_neq
        procedure z_assert_neq, s_assert_neq, d_assert_neq
    end interface assert_neq

    interface assert_approx
        procedure s_assert_approx, d_assert_approx
    end interface assert_approx

    contains

        function new_test(test_result, test_name) result(my_test)
            type(Test):: my_test
            character(len = *), optional:: test_name
            character(:), allocatable :: name
            type(Result)::test_result

            if (present(test_name)) then
                name = test_name
            else
                name = "noname"
            endif

            my_test%name = name
            my_test%result = test_result
        end function new_test

        function new_testset(test_list, name) result(my_testset)
            type(TestSet):: my_testset
            character(len = *), intent(in):: name
            type(Test), dimension(:), intent(in):: test_list

            my_testset%name = name
            my_testset%test_list = test_list
            my_testset%num_passed = 0
            my_testset%num_failed = 0
            
            num_tests = size(my_testset%test_list)

            ! TODO: Move this into testset constructor
            do i = 1, num_tests
                if (my_testset%test_list(i)%result%passed) then
                    my_testset%num_passed = my_testset%num_passed + 1
                else
                    my_testset%num_failed = my_testset%num_failed + 1
                endif
            end do 
        end function new_testset

        function assert(bool) result(my_result)
            logical, intent(in):: bool
            type(Result):: my_result
            character(len = :), allocatable:: assertion, result_msg

            if (bool) then
                assertion = "true"
            else
                assertion = "false"
            endif

            result_msg = assertion_result_msg(assertion, bool)
            my_result%result_msg = result_msg
            my_result%passed = bool
        end function assert

        function z_assert_eq(arg1, arg2) result(my_result)
            integer, intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(I4)') arg1
            write(arg2_str, '(I4)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 == arg2), "==")
        end function z_assert_eq

        function s_assert_eq(arg1, arg2) result(my_result)
            real, intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(f20.14)') arg1
            write(arg2_str, '(f20.14)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 == arg2), "==")
        end function s_assert_eq

        function d_assert_eq(arg1, arg2) result(my_result)
            DOUBLE PRECISION, intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(f20.14)') arg1
            write(arg2_str, '(f20.14)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 == arg2), "==")
        end function d_assert_eq

        function z_assert_neq(arg1, arg2) result(my_result)
            integer, intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(I4)') arg1
            write(arg2_str, '(I4)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 .ne. arg2), "!=")
        end function z_assert_neq

        function s_assert_neq(arg1, arg2) result(my_result)
            real, intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(f20.14)') arg1
            write(arg2_str, '(f20.14)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 .ne. arg2), "!=")
        end function s_assert_neq

        function d_assert_neq(arg1, arg2) result(my_result)
            DOUBLE PRECISION, intent(in):: arg1, arg2
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str

            write(arg1_str, '(f20.14)') arg1
            write(arg2_str, '(f20.14)') arg2

            my_result = build_assertion(arg1_str, arg2_str, (arg1 .ne. arg2), "!=")
        end function d_assert_neq

        function s_assert_approx(arg1, arg2, rtol, atol) result(my_result)
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
        end function s_assert_approx

        function d_assert_approx(arg1, arg2, rtol, atol) result(my_result)
            double precision, intent(in):: arg1, arg2
            double precision, optional:: rtol, atol
            double precision:: relative_tolerance, absolute_tolerance
            type(Result):: my_result
            character(len = 32):: arg1_str, arg2_str
            logical:: passed

            relative_tolerance = sqrt(EPSILON(arg1))
            absolute_tolerance = 0.0d0

            if (present(rtol)) relative_tolerance = rtol
   
            if (present(atol)) absolute_tolerance = atol

            write(arg1_str, '(f32.28)') arg1
            write(arg2_str, '(f32.28)') arg2

            passed = (abs(arg1 - arg2) .le. (absolute_tolerance + relative_tolerance * max(abs(arg1), abs(arg2))))

            my_result = build_assertion(arg1_str, arg2_str, passed, "~=")
        end function d_assert_approx
        
        function build_assertion(arg1_str, arg2_str, passed, comparision) result(my_result)
            character(len = *):: arg1_str, arg2_str, comparision
            character(len = :), allocatable:: assertion, result_msg
            type(Result):: my_result
            logical:: passed

            assertion = trim(adjustl(arg1_str))//" "//comparision//" "//trim(adjustl(arg2_str))

            result_msg = assertion_result_msg(assertion, passed)
            my_result%result_msg = result_msg
            my_result%passed = passed
        end function build_assertion

        function assertion_result_msg(assertion, passed) result(result_msg)
            character(:), allocatable, intent(in):: assertion
            character(:), allocatable:: result_msg
            logical, intent(in):: passed

            if (passed) then
                result_msg = '      Assertion "'//assertion//'" satisfied'
            else
                result_msg = '      Assertion "'//assertion//'" not satisfied'
            endif

        end function assertion_result_msg

        !function assert_neq(arg1, arg2) result(not_equals)
        !    logical::not_equals
        !    not_equals = (arg1 .ne. arg2)
        !end function assert_neq

        !subroutine run_test(my_test)
        !    type(Test), intent(inout):: my_test
        !    
        !    my_test%result = my_test%test_function()
        !end subroutine run_test

        !subroutine run_and_print(my_test, test_number)
        !    type(Test), intent(inout):: my_test
        !    integer, intent(in):: test_number
        !
        !    call run_test(my_test)
        !    call test_result_msg(my_test, test_number)
        !end subroutine run_and_print

        subroutine test_result_msg(my_test, test_number)
            type(Test), intent(in)::my_test
            integer, intent(in):: test_number
            character(len = :), allocatable:: output_string, test_name_string
            character(len = 2):: test_number_string

            write(test_number_string, '(I2)') test_number

            if (my_test%name == "noname") then
                test_name_string = '   Test '//test_number_string
            else
                test_name_string = '   Test '//test_number_string//': "'//my_test%name//'"'
            endif

            if (my_test%result%passed) then
                output_string = test_name_string//" passed."
            else
                output_string = achar(27)//'[31m'//test_name_string//' failed.'//achar(27)//'[0m'//NEW_LINE('A')// &
                                "       "//my_test%result%result_msg
            endif

            print *, output_string
        end subroutine test_result_msg

        subroutine print_testset_results(my_testset)
            character(len = 8):: num_passed_string, num_failed_string, total_string
            type(TestSet), intent(in) :: my_testset
            integer:: num_tests = 0

            write(num_passed_string, '(I2)') my_testset%num_passed
            write(num_failed_string, '(I2)') my_testset%num_failed
            write(total_string, '(I2)') my_testset%num_passed + my_testset%num_failed

            if (my_testset%num_failed > 0) then
                print *, my_testset%name//'|'//& 
                     achar(27)//'[92m'//adjustr(num_passed_string)//achar(27)//'[0m'//&
                     achar(27)//'[31m'//adjustr(num_failed_string)//achar(27)//'[0m'//&
                     achar(27)//'[96m'//adjustr(total_string)//achar(27)//'[0m'
                num_tests = size(my_testset%test_list)
                do i = 1, num_tests
                    call test_result_msg(my_testset%test_list(i), i)
                end do 
            else
                print *, my_testset%name//'|'//& 
                     achar(27)//'[92m'//adjustr(num_passed_string)//achar(27)//'[0m'//&
                     "        "// & 
                     achar(27)//'[96m'//adjustr(total_string)//achar(27)//'[0m'
            endif

            ! TODO: if any are fails, then print all tests in set
            ! TODO: better alignment using tab characters??? or fixed length characters????
        end subroutine print_testset_results

        subroutine print_all_test_results(testsets)
            type(TestSet), dimension(:), intent(in):: testsets
            print *, achar(27)//'[1m'//'Test summary:   '//achar(27)//'[0m|'//& 
                     achar(27)//'[1m'//achar(27)//'[92m'//'  Passed'//achar(27)//'[0m'//achar(27)//'[0m'//&
                     achar(27)//'[1m'//achar(27)//'[31m'//'  Failed'//achar(27)//'[0m'//achar(27)//'[0m'//&
                     achar(27)//'[1m'//achar(27)//'[96m'//'   Total'//achar(27)//'[0m'//achar(27)//'[0m'
                     
            num_testsets = size(testsets)

            do i = 1, num_testsets
                call print_testset_results(testsets(i))
            end do

        end subroutine print_all_test_results

end module