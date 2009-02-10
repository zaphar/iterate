compile: 
	erl -make
	
clean:
	rm -rf ./ebin/*.beam

boot:
	@(cd support/; $(MAKE))

test:
	@(cd t/; $(MAKE))
	@(cd t/; $(MAKE) test)
