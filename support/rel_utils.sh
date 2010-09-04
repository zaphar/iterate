#!/bin/bash

function get_mods() {
  for mod in $(find ${1}/*.erl); do
    basename $mod | sed 's/\.erl//';
  done
}

function mk_mod_list() {
  local first=$1
  shift
  local mods=$@
  echo "        ${first}"
  for mod in $mods; do
      echo "        , ${mod}"
  done
}

function mk_app_file() {
    local vsn=$1
    shift
    local mods=$@
    local mod_list=$(mk_mod_list $mods)
    #echo "mods: $mods" 1>&2
    #echo "mod_list: $mod_list" 1>&2
    
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
