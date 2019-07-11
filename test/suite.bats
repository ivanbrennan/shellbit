#! /usr/bin/env bats

setup() {
    cp -r "$SEED" "$SAND"
    cd "$LOCAL_PROJECT"
}

teardown() {
    cd "$ORIG_PWD"
    rm -rf "$SAND"
}

pseudo_tty() {
    # Because the program we're testing uses `read -p` to prompt for input,
    # the prompt will only be shown if input is coming from a terminal.
    #
    # We're using redirection to provide test input, but we still want a
    # prompt to be displayed so we can verify that we were prompted.
    #
    # Use socat to generate a pseudo terminal and run the program with
    # input coming from the pty.
    socat - EXEC:"$*",pty,setsid,ctty
}

colorstrip() {
    perl -MTerm::ANSIColor=colorstrip -ne 'print colorstrip($_)'
}

@test 'it shows Usage if --help option is used' {
    "$EXECUTABLE" --help 2>&1 \
        | grep -F 'Usage'
}

@test 'it prompts for NIX_SHELL_BIT_URL if it cannot be detected' {
    local url="$REMOTE_SHELLS"
    local out

    rm "$XDG_CONFIG_HOME/nix-shell-bit/config.dhall"

    out=$(
        pseudo_tty "$EXECUTABLE" \
            < <(echo "$url" && echo '') \
            2>&1 | colorstrip
    )

    grep -F 'NIX_SHELL_BIT_URL not found'    <<< "$out"
    grep -F 'Please enter NIX_SHELL_BIT_URL' <<< "$out"
}

@test 'it enters a development environment' {
    "$EXECUTABLE" \
        | grep -F "Entered PROJECT 0.1.0 environment"
}

@test 'it can pass additional options to nix-shell' {
    AUTO= "$EXECUTABLE" -- --run 'echo "version: $version"' \
        | grep -F "version: 0.1.0"
}

@test 'it can use a specific NIX_SHELL_BIT_BRANCH' {
    git -C "$REMOTE_SHELLS" checkout -b fnord
    cat > "$REMOTE_SHELLS/default.nix" <<'EOF'
{
  PROJECT = (import <nixpkgs> {}).mkShell {
    shellHook = ''
      echo "I am fnord"
      exit
    '';
  };
}
EOF
    git -C "$REMOTE_SHELLS" commit --all --quiet -m 'fnord'

    NIX_SHELL_BIT_BRANCH=fnord \
        "$EXECUTABLE" \
        | grep -F 'I am fnord'
}
