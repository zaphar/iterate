#!/bin/bash

function get_mods() {
  for mod in $(find ${1}/*.erl); do
    basename $mod | sed 's/\.erl//';
  done
}

function mk_mod_list() {
  first=$1
  shift
  echo "        ${mod}"
  for mod in $@; do
      echo "        , ${mod}"
  done
}

function mk_app_file() {
    vsn=$1
    shift
    mods=$@
    mod_list=$(mk_mod_list $mods)

cat << EOF
{application, iterate, [
    {description,  "Iterate! project management"},
    {id, "iterate"},
    {vsn, "${vsn}"},
    {modules, [
${mod_list}
     ]},
    {applications, []},
    {registered, [
        iterate,
        iterate_stats,
        iterate_logger,
        wf_session_sup,
        wf_session_server,
        quickstart_sup
    ]},
    {mod, {iterate_app, []}},
    {env, [
        {platform, mochiweb}, %% {inets|yaws|mochiweb}
        {port, 8001},
        {session_timeout, 20},
        {sign_key, "SIGN_KEY"},
        {www_root, "./wwwroot"},
        {log_file, "iterate.log"}
    ]}
]}.
EOF
}
