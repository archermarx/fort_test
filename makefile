# Find all source files, create a list of corresponding object files
TEST_SRCS=runtests.f90 fort_test.f90 my_tests.f90
TEST_OBJS=$(patsubst %.f90,%.o,$(TEST_SRCS))

# Ditto for mods (They will be in both lists)
TEST_MODS=fort_test.f90 my_tests.f90 
TEST_MOD_OBJS=$(patsubst %.f90,%.o,$(TEST_MODS))

# Compiler/Linker settings
FC = gfortran
FLFLAGS = -g
FCFLAGS = -g -c -Wall -Wextra -Wconversion -Og -pedantic -fcheck=bounds -fmax-errors=5
TEST_PROGRAM = runtests
TEST_OBJ = $(TEST_PROGRAM).o

# make the runtests executable
test: $(TEST_PROGRAM)

# Compiler steps for all objects
$(TEST_OBJS) : %.o : %.f90
	$(FC) $(FCFLAGS) -o $@ $<

# Linker
$(TEST_PROGRAM) : $(TEST_OBJS)
	$(FC) $(FLFLAGS) -o $@ $^

# If something doesn't work right, have a 'make debug' to 
# show what each variable contains.
debug:
	@echo "TEST_SRCS = $(TEST_SRCS)"
	@echo "TEST_OBJS = $(TEST_OBJS)"
	@echo "TEST_MODS = $(TEST_MODS)"
	@echo "TEST_MOD_OBJS = $(TEST_MOD_OBJS)"
clean:
	rm -rf $(TEST_OBJS) $(TEST_PROGRAM) $(patsubst %.o,%.mod,$(TEST_MOD_OBJS))

.PHONY: debug default clean

# Dependencies

# Main program depends on all modules
$(TEST_OBJ) : $(TEST_MOD_OBJS)

my_tests.o : fort_test.o