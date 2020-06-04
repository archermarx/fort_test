program runtests
    
    use my_tests
    use fort_test
    
    implicit none

    type(TestSet):: testset_1, testset_2
    type(TestSet), dimension(:), allocatable:: tests
    type(Result):: my_result

    testset_1 = new_testset(            &
        (/                              &
            new_test(                   &
                true(),                   &
                "True test"             &
            ),                          &
            new_test(                   &
                false(),                &
                "False test"            &
            ),                          &
            new_test(                   &
                four_equals_four(),       &   
                "2 + 2 = 4"             &
            )                           &
        /),                             &
        "My testset"                    &  
    )

    testset_2 = new_testset(            &
        (/                              &
            new_test(                   &
                true(),                 &
                "True test"             &
            ),                          &
            new_test(                   &
                four_equals_four(),     &   
                "2 + 2 = 4"             &
            ),                          &
            new_test(                   &
                false()                 &
            ),                           &
            new_test(                   &
                assert_neq(2 + 2, 4)                &
            )                           &
        /),                             &
        "2nd testset"                   & 
    )                                   

    tests = (/ testset_1, testset_2/)
    call print_all_test_results(tests)

    !my_result = assert_neq(2 + 2, 4)
    !print*, my_result%result_msg

    !my_result = assert_neq(2. + 2., 4.)
    !print*, my_result%result_msg

    !my_result = assert_neq(2.0d0 + 2.0d0, 4.0d0)
    !print*, my_result%result_msg

    !my_result = assert_approx(2.0d0 + 2.0d0, 4.0d0 + sqrt(EPSILON(4.0d0)))
    !print*, my_result%result_msg
end program