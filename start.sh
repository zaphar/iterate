#!/bin/sh
cd `dirname $0`

ERL_LIBS=deps
echo Starting Nitrogen.
erl \
	-name nitrogen@localhost \
	-pa ./ebin -pa ./include \
	-s make all \
    -boot iterate
