#!/bin/sh
cd `dirname $0`

echo Starting Nitrogen.
erl \
	-name nitrogen@someplace.net \
	-pa ./ebin -pa ./include \
	-pa ./deps/mochiweb-src/ebin -pa ./deps/mochiweb-src/include \
	-pa ./deps/nitrogen-src/ebin -pa ./deps/nitrogen-src/include \
	-s make all \
    -s reloader \
	-eval "application:start(iterate)"
