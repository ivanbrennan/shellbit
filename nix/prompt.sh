_build_prompt() {
    local green black bold normal line1 line2

    green="\033[0;32m"
    black="\033[0;30m"
    bold="\033[1m"
    normal="\033[0m"

    line1="╭\[${bold}\]\w\[${normal}\]\$(_git_ps1_)\[${normal}\] \[${black}\]\t\[${normal}\]"
    line2="╰(\u:\[${green}\]\[${bold}\]nix\[${normal}\])• "

    PS1="\n${line1}\n${line2}"
}

_git_ps1_() {
    local green="\033[0;32m"
    local normal="\033[0m"

    # __git_ps1 inserts the current git branch where %s is
    __git_ps1 " (${green}%s${normal})" 2>/dev/null
}

_build_prompt

unset _build_prompt
