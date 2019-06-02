#!/usr/bin/env bash

_nix_shell_bit() {
    local word=$2 prev=$3
    local IFS=$'\n'

    local opts=(
        --project=
        --version=
        --list
        --help
    )

    if [[ "$word" == -* ]]
    then
        opts=( "${opts[@]/%/ }" )   # append a trailing space to each option
        opts=( "${opts[@]/%= /=}" ) # except the ones that end with '='
        compopt -o nospace
        COMPREPLY=( $(compgen -W "${opts[*]}" -- "$word") )
    else
        COMPREPLY=()
    fi
}

complete -o bashdefault -o default -F _nix_shell_bit nix-shell-bit
