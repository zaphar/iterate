#!/bin/sh
cd `dirname $0`

echo Starting Nitrogen.
erl \
	-name nitrogen@localhost \
	-pa ./ebin -pa ./include \
	-pa ./deps/mochiweb/ebin -pa ./deps/mochiweb/include \
	-pa ./deps/nitrogen/ebin -pa ./deps/nitrogen/include \
	-s make all \
	-eval "application:start(iterate)"
