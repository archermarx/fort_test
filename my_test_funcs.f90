module my_test_funcs
    use tests

    contains
        function four_equals_four() result(test_result)
            logical:: test_result
            test_result = ((2 + 2) == 4)
        end function four_equals_four

        function true() result(trueval)
            logical:: trueval
            trueval = .true.
        end function true
        
        function false() result(falseval)
            logical::falseval
            falseval = .false.
        end function false
end module my_test_funcs