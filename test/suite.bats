#! /usr/bin/env bats

setup() {
    cp -r "$SANDBOX/NIX_SHELL_BIT_URL"{.seed,}
    cp -r "$SANDBOX/XDG_CONFIG_HOME"{.seed,}
    cp -r "$SANDBOX/locals/PROJECT"{.seed,}
    cd "$SANDBOX/locals/PROJECT"
}

teardown() {
    cd "$ORIG_PWD"
    rm -rf "$SANDBOX/locals/PROJECT"
    rm -rf "$SANDBOX/XDG_CONFIG_HOME"
    rm -rf "$SANDBOX/NIX_SHELL_BIT_URL"
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
    "$TOPLEVEL/bin/nix-shell-bit" --help 2>&1 \
        | grep -F 'Usage'
}

@test 'it returns zero exit code if --help option is used' {
    "$TOPLEVEL/bin/nix-shell-bit" --help
}

@test 'it shows error if PROJECT cannot be detected' {
    rm -rf "$SANDBOX/locals/PROJECT/.git"
    "$TOPLEVEL/bin/nix-shell-bit" 2>&1 \
        | tail -3 \
        | grep -iF 'detect PROJECT'
}

@test 'it returns non-zero exit code if PROJECT cannot be detected' {
    rm -rf "$SANDBOX/locals/PROJECT/.git"
    ! "$TOPLEVEL/bin/nix-shell-bit"
}

@test 'it shows error if VERSION cannot be detected' {
    rm "$SANDBOX/locals/PROJECT/VERSION"
    "$TOPLEVEL/bin/nix-shell-bit" 2>&1 \
        | tail -3 \
        | grep -iF 'detect VERSION'
}

@test 'it returns non-zero exit code if VERSION cannot be detected' {
    rm "$SANDBOX/locals/PROJECT/VERSION"
    ! "$TOPLEVEL/bin/nix-shell-bit"
}

@test 'it prompts for NIX_SHELL_BIT_URL if it cannot be detected' {
    local url="$SANDBOX/NIX_SHELL_BIT_URL"
    local out

    out=$(
        NIX_SHELL_BIT_URL='' \
            pseudo_tty "$TOPLEVEL/bin/nix-shell-bit" --list \
            < <(echo "$url" && echo '') \
            2>&1 | colorstrip
    )

    grep -F 'NIX_SHELL_BIT_URL not found'    <<< "$out"
    grep -F 'Please enter NIX_SHELL_BIT_URL' <<< "$out"
}

@test 'it saves config by default' {
    local config="$XDG_CONFIG_HOME/nix-shell-bit/env"
    local url="$SANDBOX/NIX_SHELL_BIT_URL"

    rm "$config" && rmdir "${config%/*}"

    "$TOPLEVEL/bin/nix-shell-bit" --list \
        < <(echo "$url" && echo '')

    grep -F ": \"\${NIX_SHELL_BIT_URL:=$url}\"" "$config"
}

@test 'it lets user decline saving config' {
    local url="$SANDBOX/NIX_SHELL_BIT_URL"
    local config="$XDG_CONFIG_HOME/nix-shell-bit/env"

    rm "$config" && rmdir "${config%/*}"

    "$TOPLEVEL/bin/nix-shell-bit" --list \
        < <(echo "$url" && echo 'no')

    ! test -f "$config"
}

@test 'it reprompts about saving config if answer is invalid' {
    local url="$SANDBOX/NIX_SHELL_BIT_URL"
    local reprompt="Please answer y or n "
    local config="$XDG_CONFIG_HOME/nix-shell-bit/env"

    rm "$config" && rmdir "${config%/*}"

    out=$(
        pseudo_tty "$TOPLEVEL/bin/nix-shell-bit" --list \
            < <(echo "$url" && echo 'fnord' && echo 'yes') \
            2>&1
    )

    grep -z "Save config to $XDG_CONFIG_HOME/.*Please answer y or n" <<< "$out"
    test -f "$config"
}

@test 'it returns non-zero exit code if NIX_SHELL_BIT_URL cannot be read' {
    ! NIX_SHELL_BIT_URL=/dev/null \
        "$TOPLEVEL/bin/nix-shell-bit"
}

@test 'it returns non-zero exit code if NIX_SHELL_BIT_BRANCH cannot be read' {
    ! NIX_SHELL_BIT_BRANCH=fnord \
        "$TOPLEVEL/bin/nix-shell-bit"
}

@test 'it shows error if no versions are available for PROJECT' {
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag --delete PROJECT-0.1.0
    "$TOPLEVEL/bin/nix-shell-bit" 2>&1 \
        | tail -1 \
        | grep -iF 'no versions'
}

@test 'it returns non-zero exit code if no versions are available for PROJECT' {
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag --delete PROJECT-0.1.0
    ! "$TOPLEVEL/bin/nix-shell-bit"
}

@test 'it shows error if VERSION is unavailable' {
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag --delete PROJECT-0.1.0 >/dev/null
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag PROJECT-0.0.5
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag PROJECT-0.0.6

    "$TOPLEVEL/bin/nix-shell-bit" 2>&1 \
        | grep -iF '0.1.0 not found'
}

@test 'it lists available versions if VERSION is unavailable' {
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag --delete PROJECT-0.1.0 >/dev/null
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag PROJECT-0.0.5
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag PROJECT-0.0.6

    local out
    out=$("$TOPLEVEL/bin/nix-shell-bit" 2>&1 || true)

    grep '\<0\.0\.5\>' <<< "$out"
    grep '\<0\.0\.6\>' <<< "$out"
}

@test 'it returns non-zero exit code if VERSION is unavailable' {
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag --delete PROJECT-0.1.0 >/dev/null
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag PROJECT-0.0.5
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag PROJECT-0.0.6

    ! "$TOPLEVEL/bin/nix-shell-bit"
}

@test 'it lists available versions if --list is used' {
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag PROJECT-0.0.9
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag PROJECT-0.1.1

    local out
    out=$("$TOPLEVEL/bin/nix-shell-bit" --list 2>&1 | colorstrip)

    grep '\<0\.0\.9\>' <<< "$out"
    grep '\<0\.1\.0\>' <<< "$out"
    grep '\<0\.1\.1\>' <<< "$out"
}

@test 'it returns zero exit code after listing available versions' {
    "$TOPLEVEL/bin/nix-shell-bit" --list
}

@test 'it does not warn about unavailable VERSION if --list is used' {
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag --delete PROJECT-0.1.0 >/dev/null
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag PROJECT-0.0.9
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" tag PROJECT-0.1.1

    local out
    out=$("$TOPLEVEL/bin/nix-shell-bit" --list 2>&1)

    ! grep -iF '0.1.0 not found' <<< "$out"
}

@test 'it is not an error when VERSION cannot be detected if --list is used' {
    rm "$SANDBOX/locals/PROJECT/VERSION"
    "$TOPLEVEL/bin/nix-shell-bit" --list
}

@test 'it enters the development environment' {
    "$TOPLEVEL/bin/nix-shell-bit" \
        | grep -F "Entered PROJECT 0.1.0 environment"
}

@test 'it can pass additional options to nix-shell' {
    AUTO= "$TOPLEVEL/bin/nix-shell-bit" --run 'echo "version: $version"' \
        | grep -F "version: 0.1.0"
}

@test 'it can use a specific NIX_SHELL_BIT_BRANCH' {
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" checkout -b fnord
    cat > "$SANDBOX/NIX_SHELL_BIT_URL/default.nix" <<'EOF'
{
  PROJECT = (import <nixpkgs> {}).mkShell {
    shellHook = ''
      echo "I am fnord"
      exit
    '';
  };
}
EOF
    git -C "$SANDBOX/NIX_SHELL_BIT_URL" commit --all --quiet -m 'fnord'

    NIX_SHELL_BIT_BRANCH=fnord \
        "$TOPLEVEL/bin/nix-shell-bit" \
        | grep -F 'I am fnord'
}
