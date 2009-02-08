compile: 
	erl -make
	
clean:
	rm -rf ./ebin/*.beam

test:
	@(cd t/; $(MAKE))
	@(cd t/; $(MAKE) test)
