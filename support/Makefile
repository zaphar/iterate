include ../include/globals.mk
 
all:
	ERL_LIBS=../deps erl -pa ../ebin -run  systools make_script iterate -run init stop -noshell
	mv *.boot ../

tar:
	ERL_LIBS=../deps erl -pa ../ebin -run  systools make_tar iterate -run init stop -noshell
       
clean:
	rm -f iterate.boot iterate.script *.tgz *.tar.gz
