#!/bin/sh
cd `dirname $0`

echo Starting Nitrogen.
erl \
	-name nitrogen@localhost \
	-pa ./ebin -pa ./include \
	-pa ./deps/mochiweb-src/ebin -pa ./deps/mochiweb-src/include \
	-pa ./deps/nitrogen-src/ebin -pa ./deps/nitrogen-src/include \
	-s make all \
	-eval "application:start(iterate)"
