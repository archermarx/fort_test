module my_tests
    use fort_test

    implicit none 

    contains
        function four_equals_four() result(test_result)
            type(Result):: test_result
            test_result = assert_eq(2+2, 4)
        end function four_equals_four

        function true() result(test_result)
            type(Result):: test_result
            test_result = assert(.true.)
        end function true
        
        function false() result(test_result)
            type(Result):: test_result
            test_result = assert(.false.)
        end function false
end module