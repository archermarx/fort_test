[![Build Status](https://travis-ci.com/archermarx/fort_test.svg?branch=master)](https://travis-ci.com/archermarx/fort_test) 
[![codecov](https://codecov.io/gh/archermarx/fort_test/branch/master/graph/badge.svg)](https://codecov.io/gh/archermarx/fort_test)

# fort_test

Very lightweight testing framework for FORTRAN, written entirely in FORTRAN. Supports basic assertions and running of test sets. 

## Installation
Clone the fort_test repo into your project directory

## Usage

Simply type

``` f95
use fort_tests
```

At the top of your file. You can make a runtests.f90 file that can compile to a runtests executable to perform all of your tests without compiling the main program you're working on. I have included a basic makefile and example runtests.f90 file to show how you could do this

To make a test file, first declare at least one `TestSet` structure and (optionally) a named array of `TestSet`s. 

```f95
type(TestSet):: testset_1, testset_2
type(TestSet), dimension(:), allocatable:: my_testsets
```

Next, use the `new_testset` constructor to build (and optionally name) your testsets and fill them with tests. To see the results of your tests, call the `print_results` subroutine, which takes an array of `TestSets` as an argument

```f95
testset_1 = new_testset(    &
  (/                        &
    assert(.true.),         &
    assert_eq(2.0, 1.0 + 1.0),  &
    assert_neq(2.0, 2.0 + 2.0), &
    assert_approx(4.d0, 4.d0 + 10.d0*epsilon(4.d0)) &
  /),                       &
  name = "Sample test set"  &
)

my_testsets = (/ testset_1 /)
call run_and_exit(my_testsets)
```

All of our tests are self evidently correct, so we should get the following output, all nicely colored:

![Test output 1][Passing tests]

Lets make some tests that fail now. We'll copy our first testset and make all of the tests fail.

```f95
testset_2 = new_testset(    &
  (/                        &
    assert(.false.),         &
    assert_eq(3.0, 1.0 + 1.0),  &
    assert_neq(4.0, 2.0 + 2.0), &
    assert_approx(3.d0, 4.d0 + 10.d0*epsilon(4.d0)) &
  /),                       &
  name = "Failing test set"  &
)

my_testsets = (/ testset_1, testset_2 /)
call run_and_exit(my_testsets)
```

![Test output 2][Failing tests]

The program provides minimal but helpful messages here. It doesn't have the ability to read the line of sourcecode that produced the error so we can only use whatever arguments you pass in. The message for a failing test using the basic 'assert' function will always be pretty sparse, but the test numbers will help you figure out which line of code is failing. Use the other assertion functions if you want more detailed readout.

That about concludes the basic tutorial. More documentation will be coming in the future! Please let me know if you have any questions or if you have functionality you'd like included. 

[Passing tests]: https://i.ibb.co/VJLQpMk/test-pic-1.png
[Failing tests]: https://i.ibb.co/VNJ9b5d/test-pic-2.png
