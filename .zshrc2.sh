
PRIVATE_DOTFILES="${HOME}/.local/share/private"
ZSH_CUSTOM="${HOME}/.zsh.d"
OMZSH="${HOME}/.local/share/ohmyzsh"
ZSH=$OMZSH  # the variable that oh-my-zsh expects

source ${ZSH_CUSTOM}/common.sh

case $(id -u -n) in
    ben)
        ENV="sudo.is"
        if [[ -d "${PRIVATE_DOTFILES}" ]]; then
            SUDO_ENV="shell"
        elif [[ -f "/usr/local/etc/sudoisbot.yml" ]]; then
            SUDO_ENV="sensor"
        else
            SUDO_ENV="server"
        fi
        export ZELLIJ_SESSION_NAME="default"
        ;;
    benedikt.kristinsson)
        ENV="care.com"
        # set $CARE_ENV
        source ${PRIVATE_DOTFILES}/.zsh.d/care-env.sh

        export ZELLIJ_SESSION_NAME="${CARE_ENV}"
        settitle "${CARE_ENV}"
        ;;
    *)
        ENV="unknown"
        ;;
esac

# Environment settings
unset MAILCHECK

export EDITOR="vim"
export PATH="${HOME}/.local/bin:/usr/local/sbin:/usr/local/bin/:/usr/bin:/usr/sbin:/bin:/sbin"
export TZ="Europe/Berlin"
export TERM="xterm-256color"

export ZELLIJ_AUTO_ATTACH="true"
export GPG_TTY=$(tty)
export HATCH_INTERACTIVE=false
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export DOTNET_CLI_TELEMETRY_OPTOUT=true

SSH_AGENT_ENVFILE="${HOME}/.cache/ssh-agent.env"

## Change defaults from oh-my-zsh
### oh-my-zsh will set this var otherwise, causing e.g. awscli to display everything in a pager
### https://superuser.com/questions/1698521/zsh-keep-all-command-outputs-on-terminal-screen
export PAGER=""
export UPDATE_ZSH_DAYS=31
COMPLETION_WAITING_DOTS="true" # print red dots while waiting for completion

## Rust and Cargo
if [[ -d "${HOME}/.cargo/bin" ]]; then
    if [[ -d "${HOME}/.rustup" ]]; then
        export RUSTUP_HOME="${HOME}/.rustup"
    fi
    export PATH="${HOME}/.cargo/bin:$PATH"
    export CARGO_HOME="${HOME}/.cargo"
fi
## Homebrew
if [[ -d "/opt/homebrew/bin" ]]; then
    PATH="$PATH:/opt/homebrew/bin"
fi
## Emacs
if [[ "$EDITOR" == "emacs" ]]; then
    source $PRIVATE_DOTFILES/zsh.d/emacs.sh
fi
if [[ "$SUDO_ENV" == "sensor" ]]; then
    PROMPT="🌡️   $PROMPT"
fi

# Aliases
alias less="less -R"
alias df='df -x tmpfs -x devtmpfs -x efivarfs -h'
alias dmesg="dmesg --human --color=always -T"
alias nomail="echo 'd *' | mail -N"
## from jbs
alias json2yaml="python -c 'import sys, yaml, json; yaml.safe_dump(json.load(sys.stdin), sys.stdout, default_flow_style=False)'"
alias j2y="json2yaml"

alias private='git -C ~/.local/share/private'
alias myip='curl -sS https://www.sudo.is/api/myip | jq .'

if command -v "eza" >/dev/null; then
    # --total-size
    alias ls="eza -1 --icons=never --color=always --classify=always --color-scale=all --color-scale-mode=gradient --group-directories-first --git-repos-no-status --git --no-user --no-permissions --octal-permissions --header"
    alias tree="eza --tree --color=never --icons=always"
fi
if command -v "bat" >/dev/null; then
    alias cat="bat -p"
fi
if command -v "rg" >/dev/null; then
    alias grep="rg"
fi

## MacOS
if [[ "${OSTYPE}" == "darwin"* ]]; then
    alias speed="sudo pmset -a disablesleep 1"
    alias weed="sudo pmset -a disablesleep 0"
fi

# Load things

# https://github.com/NixOS/nix/issues/7880#issuecomment-1750399157
if [[ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi

## Safely load ssh-agent without eval
if [[ -f "${SSH_AGENT_ENVFILE}" ]]; then
    chmod 700 $SSH_AGENT_ENVFILE
    # exec $(cat $SSH_AGENT_ENVFILE)
    source $SSH_AGENT_ENVFILE > /dev/null
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID
fi

## MOTD with env info and ssh-agent keys
if [[ -S ${SSH_AUTH_SOCK}  && -n "${SSH_AGENT_PID}" ]] && `ps -p "${SSH_AGENT_PID}" >/dev/null`; then
    # -S: is a socket file
    motd_ssh_agent=$(while IFS= read -r line; do
        echo -e "${color_purple}ssh-agent${color_nc}   ${color_orange}${line}${color_nc}"
    done) <<< $(ssh-add -l | awk -F' ' '{ print $3 }')
elif [[ -f $SSH_AGENT_ENVFILE ]]; then
    rm -v $SSH_AGENT_ENVFILE
fi


if [[ "${MOTD_SHOW}" != "false" ]]; then
    motd_env="${color_purple}env${color_nc}         ${ENV_COLOR}${ENV}${color_nc}"
    echo -e "$motd_env"
    if [[ -n "${SSH_CLIENT}" ]]; then
        echo -ne "${color_purple}from ipv4${color_nc}   ${color_cyan}"
        echo -n $SSH_CLIENT | awk '{ print $1 }'
        echo -e "${color_nc}"
    fi
    if [[ -n "$motd_ssh_agent" ]]; then
        echo -e "$motd_ssh_agent"
    fi
fi

## oh-my-zsh plugins
if [[ "${OHZSH_DISABLED}" != "true" ]]; then
    plugins=(
        colored-man-pages
        docker
        git
        safe-paste
    )
    source $ZSH/oh-my-zsh.sh
else
    # on a system without oh-my-zsh (f.ex. non util/control nodes)
    # set some basic settings
    SAVEHIST=10000
    HISTFILE=~/.zsh_history
fi
