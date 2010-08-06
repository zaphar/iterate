include ../include/globals.mk
 
all:
	erl -pa ../ebin -run  systools make_script iterate -run init stop -noshell

tar:
	erl -pa ../ebin -run  systools make_tar iterate -run init stop -noshell
       
clean:
	rm -f iterate.boot iterate.script
