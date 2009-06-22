#!/bin/sh
cd $(dirname $0)

echo Starting Nitrogen.
ERL_LIBS=deps erl \
	-name nitrogen@$(hostname) \
	-pa ./ebin -pa ./include \
	-s make all \
	-s reloader \
	-boot iterate \
    -async_shell_start
