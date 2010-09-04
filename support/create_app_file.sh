#!/bin/bash
wd=$(dirname $0)
root=${wd}/..

source ${wd}/rel_utils.sh

mk_app_file rc5 $(get_mods ${root}/src | sort) > ${root}/ebin/iterate.app
