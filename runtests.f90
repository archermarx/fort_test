program runtests
    
    use my_tests
    use fort_test
    
    implicit none

    type(TestSet):: testset_1, testset_2
    type(TestSet), dimension(:), allocatable:: tests
    type(Result):: my_result

    testset_1 = new_testset(            &
        (/                              &
            assert(.true.),             &               
            assert_eq(2+2, 4),           &
            assert_approx(4.0d0, 4.0d0 + sqrt(epsilon(4.0d0))) &
        /),                             &
        "My testset"                    &  
    )

    testset_2 = new_testset(            &
        (/                              &
            assert(.true.),              &
            assert_neq(2+2, 4)          &
        /),                             &
        "2nd testset"                   & 
    )                                   

    tests = (/ testset_1, testset_2/)
    call print_all_test_results(tests)
end program