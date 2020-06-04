program mytests
    
    use tests
    use my_test_funcs
    implicit none

    type(TestSet):: my_testset, my_testset_2
    type(TestSet), dimension(:), allocatable:: my_tests

    my_testset = new_testset(           &
        "My testset",                   & 
        (/                              &
            new_test(                   &
                "True test",            &
                true                    &
            ),                          &
            new_test(                   &
                "False test",           &
                false                   &
            ),                          &
            new_test(                   &
                "2 + 2 = 4",            &
                four_equals_four        &   
            )                           &
        /)                              &
    )

    my_testset_2 = new_testset(         &
        "2nd testset",                  & 
        (/                              &
            new_test(                   &
                "True test",            &
                true                    &
            ),                          &
            new_test(                   &
                "2 + 2 = 4",            &
                four_equals_four        &   
            )                           &
        /)                              &
    )                                   

    my_tests = (/ my_testset, my_testset_2/)
    call print_all_test_results(my_tests)
end program mytests