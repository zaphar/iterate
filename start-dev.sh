#!/bin/sh
cd `dirname $0`

echo Starting Nitrogen.
erl \
	-name nitrogen@someplace.net \
	-pa ./ebin -pa ./include \
	-pa ./deps/mochiweb/ebin -pa ./deps/mochiweb/include \
	-s make all \
	-s reloader \
	-eval "application:start(iterate)"
