FC = gfortran
TEST_PROGRAM = runtests
PROGRAM = main
TEST_FLAGS = --coverage -O0

EXECUTABLES = $(TEST_PROGRAM) $(PROGRAM)
SOURCE_DIR = src

TEST_FILES = fort_test.f90
TEST_MAIN = runtests.f90
PROGRAM_MAIN = $(SOURCE_DIR)/main.f90

all: 
	make default test
default: 
	$(FC) -o $(PROGRAM) $(PROGRAM_MAIN)
test: 
	$(FC) $(TEST_FLAGS) -o $(TEST_PROGRAM) $(COMMON_FILES) $(TEST_FILES) $(TEST_MAIN)
clean:
	rm -rf *.mod *.o $(EXECUTABLES)
example:
	$(FC) -o $(TEST_PROGRAM) runtests.f90 fort_test.f90