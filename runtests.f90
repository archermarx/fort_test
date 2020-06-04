program runtests
    
    use my_tests
    use fort_test
    
    implicit none

    type(TestSet):: testset_1, testset_2
    type(TestSet), dimension(:), allocatable:: tests

    testset_1 = new_testset(            &
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

    testset_2 = new_testset(            &
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

    tests = (/ testset_1, testset_2/)
    call print_all_test_results(tests)
end program