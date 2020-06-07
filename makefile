FC = gfortran
TEST_PROGRAM = runtests
TEST_FLAGS = --coverage -O0

EXECUTABLES = $(TEST_PROGRAM) $(PROGRAM)
SOURCE_DIR = src
TEST_DIR = test
DOCS_DIR = docs

TEST_FILES = $(SOURCE_DIR)/fort_test.f90
TEST_MAIN = $(SOURCE_DIR)/tests.f90

all: 
	make test
default: 
	make test
test: 
	$(FC) $(TEST_FLAGS) -o $(TEST_PROGRAM) $(TEST_FILES) $(TEST_MAIN)
clean:
	rm -rf *.mod *.o $(EXECUTABLES) *.gcda *.gcno
docs:
	doxygen Doxyfile