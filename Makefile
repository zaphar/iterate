compile: deps 
	(cd deps/nitrogen && make)
	(cd deps/mochiweb && make)
	ERL_LIBS=deps:deps/nitrogen/apps erl -make

debug: deps
	(cd deps/nitrogen && make debug)
	(cd deps/mochiweb && make)
	ERL_LIBS=deps:deps/nitrogen/apps erl -noinput -run make all debug_info -run init stop

clean:
	(cd deps/nitrogen && make clean)
	rm -rf ./ebin/*.beam

boot:
	@(cd support/; $(MAKE))

release:
	@(cd support/; $(MAKE) tar)

test:
	@(cd deps/etap && make)
	@(cd t/; $(MAKE))
	@(cd t/; $(MAKE) test)
