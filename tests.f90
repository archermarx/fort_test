module tests

    type Test
        character(len = :), allocatable:: name
        logical:: result
        procedure(testinterface), pointer, nopass :: test_function
    end type

    type TestSet
        character(len = 16):: name
        type(Test), dimension(:), allocatable:: test_list
        integer:: num_passed, num_failed
    end type

    interface
        function testinterface()
            logical::testinterface
        end function testinterface
    end interface

    contains
        function new_test(name, test_func) result(my_test)
            type(Test):: my_test
            character(len = *), intent(in):: name

            interface 
                function test_func()
                    logical::test_func
                end function test_func
            end interface

            my_test%name = name
            my_test%test_function => test_func
            call run_test(my_test)
        end function new_test

        function new_testset(name, test_list) result(my_testset)
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
                if (my_testset%test_list(i)%result) then
                    my_testset%num_passed = my_testset%num_passed + 1
                else
                    my_testset%num_failed = my_testset%num_failed + 1
                endif
            end do 
        end function new_testset

        function four_equals_four() result(test_result)
            logical:: test_result
            test_result = ((2 + 2) == 4)
        end function four_equals_four

        !function assert_eq(arg1, arg2) result(equals)
        !   logical::equals
        !    equals = (arg1 == arg2)
        !end function assert_eq

        !function assert_neq(arg1, arg2) result(not_equals)
        !    logical::not_equals
        !    not_equals = (arg1 .ne. arg2)
        !end function assert_neq

        function true() result(trueval)
            logical:: trueval
            trueval = .true.
        end function true
        
        function false() result(falseval)
            logical::falseval
            falseval = .false.
        end function false

        subroutine run_test(my_test)
            type(Test), intent(inout):: my_test
            logical:: test_result

            test_result = my_test%test_function()
            my_test%result = test_result
        end subroutine run_test

        subroutine run_and_print(my_test, test_number)
            type(Test), intent(inout):: my_test
            integer, intent(in):: test_number

            call run_test(my_test)
            call test_result_msg(my_test, test_number)
        end subroutine run_and_print

        subroutine test_result_msg(my_test, test_number)
            type(Test), intent(in)::my_test
            integer, intent(in):: test_number
            character(len = :), allocatable:: output_string
            character(len = 2):: test_number_string

            write(test_number_string, '(I2)') test_number

            if (my_test%result) then
                output_string = '   Test '//test_number_string//': "'//my_test%name//'" '//"passed"
            else
                output_string = achar(27)//'[31m   Test '//test_number_string//': "'//my_test%name//'" '//"failed"//achar(27)//'[0m'
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
end