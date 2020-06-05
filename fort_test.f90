!TODO:  1.  Allow running of tests to be deferred/ignored       
!       3.  Simpler test set declaration?

module fort_test
    
    type Result
        character(len = :), allocatable:: assertion
        logical:: passed
    end type

    type TestSet
        character(len = :), allocatable:: name
        type(Result), dimension(:), allocatable:: test_list
        integer:: num_passed, num_failed
    end type

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
        function new_testset(test_list, name) result(my_testset)
            type(TestSet):: my_testset
            character(len = *), optional:: name
            character(len = :), allocatable::testset_name
            type(Result), dimension(:), intent(in):: test_list

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
        end function new_testset

        function assert(bool) result(my_result)
            logical, intent(in):: bool
            type(Result):: my_result

            if (bool) then
                my_result%assertion = "true"
            else
                my_result%assertion = "false"
            endif

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

            write(arg1_str, '(d31.17)') arg1
            write(arg2_str, '(d31.17)') arg2

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

            write(arg1_str, '(d31.17)') arg1
            write(arg2_str, '(d31.17)') arg2

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

            write(arg1_str, '(d31.17)') arg1
            write(arg2_str, '(d31.17)') arg2

            passed = (abs(arg1 - arg2) .le. (absolute_tolerance + relative_tolerance * max(abs(arg1), abs(arg2))))

            my_result = build_assertion(arg1_str, arg2_str, passed, "~=")
        end function d_assert_approx
        
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

            if (my_result%passed) then
                output_string = test_name_string//" passed."
            else
                output_string = achar(27)//'[31m'//test_name_string//' failed.'//achar(27)//'[0m'//NEW_LINE('A')// &
                                "       "//'      Assertion "'//my_result%assertion//'" not satisfied'
            endif

            print *, output_string
        end subroutine print_result_msg

        subroutine print_testset_results(my_testset, testset_number)
            character(len = 8):: num_passed_string, num_failed_string, total_string, number_string
            character(len = 20):: name_string
            type(TestSet), intent(in) :: my_testset
            integer, intent(in):: testset_number
            integer:: num_tests = 0

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
                print *, name_string//'|'//& 
                     achar(27)//'[92m'//adjustr(num_passed_string)//achar(27)//'[0m'//&
                     achar(27)//'[31m'//adjustr(num_failed_string)//achar(27)//'[0m'//&
                     achar(27)//'[96m'//adjustr(total_string)//achar(27)//'[0m'
                num_tests = size(my_testset%test_list)
                do i = 1, num_tests
                    call print_result_msg(my_testset%test_list(i), i)
                end do 
            else
                print *, name_string//'|'//& 
                     achar(27)//'[92m'//adjustr(num_passed_string)//achar(27)//'[0m'//&
                     "        "// & 
                     achar(27)//'[96m'//adjustr(total_string)//achar(27)//'[0m'
            endif

        end subroutine print_testset_results

        subroutine print_results(testsets)
            type(TestSet), dimension(:), intent(in):: testsets
            print *, achar(27)//'[1m'//'Test summary:       '//achar(27)//'[0m|'//& 
                     achar(27)//'[1m'//achar(27)//'[92m'//'  Passed'//achar(27)//'[0m'//achar(27)//'[0m'//&
                     achar(27)//'[1m'//achar(27)//'[31m'//'  Failed'//achar(27)//'[0m'//achar(27)//'[0m'//&
                     achar(27)//'[1m'//achar(27)//'[96m'//'   Total'//achar(27)//'[0m'//achar(27)//'[0m'
                     
            num_testsets = size(testsets)

            do i = 1, num_testsets
                call print_testset_results(testsets(i), i)
            end do

        end subroutine print_results

end module