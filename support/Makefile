include ../include/globals.mk
 
all: boot
	mv *.boot ../

boot:
	ERL_LIBS=../deps erl -pa ../ebin -run systools make_script iterate -run init stop -noshell

tar: boot
	ERL_LIBS=../deps erl -pa ../ebin -run systools make_tar iterate -run init stop -noshell

target-system:
	ERL_LIBS=../deps escript install.erl create iterate

clean:
	rm -f iterate.boot iterate.script plain.rel *.tgz *.tar.gz
