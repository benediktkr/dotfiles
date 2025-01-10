
PRIVATE_DOTFILES="${HOME}/.local/share/private"
ZSH_CUSTOM="${HOME}/.zsh.d"
OMZSH="${HOME}/.local/share/ohmyzsh"
ZSH=$OMZSH  # the variable that oh-my-zsh expects

source ${ZSH_CUSTOM}/common.sh

case $(id -u -n) in
    ben)
        ENV="sudo.is"
        ;;
    benedikt.kristinsson)
        ENV="care.com"
        ;;
    *)
        if [[ -d "/meta" ]]; then
            ENV="sdf.org"
        else
            ENV="unknown"
        fi
        ;;
esac

case ${ENV} in
    sudo.is)
        PROMPT_COLOR="red"
        ENV_COLOR=$color_green
        if [[ -d "${PRIVATE_DOTFILES}" ]]; then
            SUDO_ENV="shell"
            ZSH_THEME="jreese2"
            export ZELLIJ_SESSION_NAME="default"
        elif [[ -f "/usr/local/etc/sudoisbot.yml" ]]; then
            SUDO_ENV="sensor"
            ZSH_THEME="jreese2"
        else
            SUDO_ENV="server"
            ZSH_THEME="gentoo"
        fi
        ;;
    care.com)
        source ${PRIVATE_DOTFILES}/zsh.d/care-env.sh    # set CARE_ENV
        source ${PRIVATE_DOTFILES}/zsh.d/caredotcom.sh

        ZSH_THEME="jreese2"
        PROMPT_HOSTNAME=$CARE_ENV
        ROMPT_COLOR_HOSTNAME=green
        PROMPT_COLOR=blue
        PROMPT_DELIM=":"
        ENV_COLOR=$color_green
        alias noflag='unset RPROMPT'

        settitle "${CARE_ENV}"
        export ZELLIJ_SESSION_NAME="${CARE_ENV}"
        ;;
    sdf.org)
        ZSH_THEM="jreese2"
        PROMPT_COLOR_HOSTNAME=yellow
        ENV_COLOR=$color_orange
        ;;
    *)
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
setopt shwordsplit        # for loops over "space separated strings" like bash
setopt append_history     # append rather then overwrite
setopt extended_history   # save timestamp
setopt inc_append_history # add history immediately after typing a command
setopt norcs              # disable clearing terminal after ssh logout

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
    PATH="/opt/homebrew/sbin:/opt/homebrew/bin:$PATH"
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
## sudo.is
if [[ "${ENV}" == "sudo.is" ]]; then
    if [[ "${SUDO_ENV}" == "shell" ]]; then
        alias dl-mp3='yt-dlp --extract-audio --embed-thumbnail --embed-metadata --audio-quality 320k --audio-format "mp3" --format "ba"'
        alias dl-audio='yt-dlp --extract-audio --embed-thumbnail --embed-metadata --audio-quality "best" --audio-format "best" --format "ba"'
        alias dl-audio-keep='yt-dlp --keep-video --extract-audio --embed-thumbnail --embed-metadata --audio-quality "best" --audio-format "best" --format "ba"'
    fi
    alias stopssh='ssh -O stop $(ls -1 /tmp/ssh-cm-ben*| cut -d"-" -f4 >/dev/stdout >/dev/stderr)'
    alias pullwww='(cd ~/infra && ansible-playbook www.yml --diff --tags wwwsudois,www-api,www-nginx --limit www && ansible-playbook matrix.yml --diff --tags www && ansible-playbook pirate.yml --diff --tags www)'
    alias dockps='docker ps --format "table {{.Names}}\t{{.Image}}\t{{.Status}}"'
    alias nc-occ='docker exec -it --user www-data nextcloud php occ'
    alias codec='ffprobe -v error -select_streams v:0 -show_entries stream=codec_name -of default=noprint_wrappers=1:nokey=1'
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
