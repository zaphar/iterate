escript mk_iterate_boot.erl
erl -pa ../ebin -run  systools make_script $1 -run init stop -noshell
mv $1.boot ../
