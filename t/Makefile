include ../include/globals.mk

all: $(EBIN_FILES)

clean:
	rm -rf $(EBIN_FILES) erl_crash.dump

test: $(MODULES)

./$(MODULES):
	@echo "Running tests for $@"
	@rm -rf Mnesia.*@*
	@erl -pa ../ebin -run $@ start -run init stop -noshell
