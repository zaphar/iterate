#!/bin/sh
cd `dirname $0`

ERL_LIBS=deps
echo Starting Nitrogen.
erl \
	-name nitrogen@someplace.net \
	-pa ./ebin -pa ./include \
	-s make all \
	-s reloader \
	-boot iterate \
    -async_shell_start
