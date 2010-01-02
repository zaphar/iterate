include ../include/globals.mk

all:
	ERL_LIBS=../deps erl -make

clean_db:
	rm -rf Mnesia.*@*

clean: clean_db
	rm -rf $(EBIN_FILES) erl_crash.dump
	rm -rf Mnesia.*@*

test: $(MODULES)

./$(MODULES): clean_db
	@echo "Running tests for $@"
	ERL_LIBS=../deps erl -pa ../ebin -run $@ start -run init stop -noshell
