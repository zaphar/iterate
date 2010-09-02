include ../include/globals.mk
 
all: boot
	mv *.boot ../

boot:
	ERL_LIBS=../deps erl -pa ../ebin -pa ../deps/nitrogen/ebin -run systools make_script iterate -run init stop -noshell

rel-boot:
	ERL_LIBS=../deps erl -pa ../ebin -pa ../deps/nitrogen/ebin -run systools make_script iterate-rel -run init stop -noshell

tar: rel-boot
	ERL_LIBS=../deps erl -pa ../ebin  ../deps/nitrogen/ebin -run systools make_tar iterate-rel -run init stop -noshell
       
clean:
	rm -f iterate.boot iterate.script *.tgz *.tar.gz
