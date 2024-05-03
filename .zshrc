function settitle() {
    # https://stackoverflow.com/questions/46721797/
    echo -ne "\033]0;$@\007"
}

unset MAILCHECK || true

DISABLE_AUTO_TITLE="true"

case $(id -u -n) in
    benedikt.kristinsson)
        ENV="care.com"
        PRIVATE_DOTFILES_REMOTE="https://git.sudo.is/ben/dotfiles-private"
        PRIVATE_DOTFILES="$HOME/.local/share/private"
        source $PRIVATE_DOTFILES/zsh.d/care-env.sh
        if [[ -n "$CARE_ENV" ]]; then
            settitle $CARE_ENV
        fi
        ;;
    ben)
        ENV="sudo.is"
        if [[ -d "${HOME}/.local/share/yadm/repo.git" || -d "${PRIVATE_DOTFILES}" ]]; then
            SUDO_ENV="shell"
        else
            SUDO_ENV="server"
        fi
        ;;
    *)
        ENV="unknown"
        ;;
esac

# This might need different handling on systems without yadm
ZSH_CUSTOM="${HOME}/.zsh.d"
OMZSH="${HOME}/.local/share/ohmyzsh"

SSH_AGENT_ENVFILE="${HOME}/.agent-ssh.env"

# the variable that oh-my-zsh expects
ZSH=$OMZSH

# nice omzsh themes:
#  * jreese
#  * gentoo
#  * alanpeabody (doesnt show full path and color blends with background)

color_red='\033[0;31m'
color_red_light='\033[1;31m'
color_green='\033[0;32m'
color_green_light='\033[1;32m'
color_orange='\033[0;33m'
color_yellow='\033[1;33m'
color_blue='\033[0;34m'
color_purple='\033[0;35m'
color_cyan='\033[0;36m'
color_purple='\033[0;35m'

color_nc='\033[0m'

export EDITOR="vim"
export TZ="Europe/Berlin"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export PATH="$HOME/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin"
export GPG_TTY=$(tty)
export HATCH_INTERACTIVE=false

if [[ -d "/opt/homebrew/bin" ]]; then
    PATH="$PATH:/opt/homebrew/bin"
fi

# oh-my-zsh will set this var otherwise, causing e.g. awscli to display everything in a pager
# https://superuser.com/questions/1698521/zsh-keep-all-command-outputs-on-terminal-screen
export PAGER=""

# in case dotnet happens to be installed, disable the telemetry
export DOTNET_CLI_TELEMETRY_OPTOUT=true

setopt shwordsplit # for loops over "space separated strings" like bash
setopt append_history # append rather then overwrite
setopt extended_history # save timestamp
setopt inc_append_history # add history immediately after typing a command

alias less="less -R"
alias dmesg="dmesg --human --color=always -T"
alias nomail="echo 'd *' | mail -N"
# from jbs
alias json2yaml="python -c 'import sys, yaml, json; yaml.safe_dump(json.load(sys.stdin), sys.stdout, default_flow_style=False)'"
alias j2y="json2yaml"

alias private='git -C ~/.local/share/private'
alias myip='curl -sS https://www.sudo.is/api/myip | jq .'

if [[ "$ENV" == "care.com" ]]; then
    ENV="care.com"
    ZSH_THEME="jreese2"

    source ${PRIVATE_DOTFILES}/zsh.d/caredotcom.sh

    # set colors for jreese2
    export PROMPT_HOSTNAME=$CARE_ENV
    export PROMPT_COLOR_HOSTNAME=green
    export PROMPT_COLOR=blue
    ENV_COLOR=$color_green
    alias noflag="unset RPROMPT"


elif [[ -d "/meta" || -d "/sdf" ]]; then
    ENV="sdf.org"
    ZSH_THEME="jreese2"

    export PROMPT_COLOR_HOSTNAME=yellow
    export PROMPT_COLOR=blue
    ENV_COLOR=$color_orange
else
    ENV="sudo.is"
    if [[ "$SUDO_ENV" == "server" ]]; then
        PROMPT_COLOR=blue
        ZSH_THEME="gentoo"
    else
        if [[ "${OSTYPE}" == "darwin"* ]]; then
            PROMPT_COLOR="magenta"
        fi
        ZSH_THEME="jreese2"
    fi
    if [[ -d "$HOME/.cargo" ]]; then
        export PATH="$HOME/.cargo/bin:$PATH"
    fi

    alias dl-mp3='yt-dlp --extract-audio --embed-thumbnail --embed-metadata --audio-quality 320k --audio-format "mp3" --format "ba"'
    alias dl-audio='yt-dlp --extract-audio --embed-thumbnail --embed-metadata --audio-quality "best" --audio-format "best" --format "ba"'
    alias dl-audio-keep='yt-dlp --keep-video --extract-audio --embed-thumbnail --embed-metadata --audio-quality "best" --audio-format "best" --format "ba"'

    alias stopssh='ssh -O stop $(ls -1 /tmp/ssh-cm-ben*| cut -d"-" -f4 >/dev/stdout >/dev/stderr)'

    alias pullwww='(cd ~/infra && ansible-playbook www.yml --diff --tags wwwsudois,www-api,www-nginx --limit www && ansible-playbook matrix.yml --diff --tags www && ansible-playbook pirate.yml --diff --tags www)'
    alias dockps='docker ps --format "table {{.Names}}\t{{.Image}}\t{{.Status}}"'
    alias nc-occ='docker exec -it --user www-data nextcloud php occ'
    alias codec='ffprobe -v error -select_streams v:0 -show_entries stream=codec_name -of default=noprint_wrappers=1:nokey=1'
fi
motd_env="${color_purple}env${color_nc}         ${ENV_COLOR}${ENV}${color_nc}"

# safely load ssh-agent without eval
if [[ -f "${SSH_AGENT_ENVFILE}" ]]; then
    chmod 700 $SSH_AGENT_ENVFILE
    # exec $(cat $SSH_AGENT_ENVFILE)
    source $SSH_AGENT_ENVFILE > /dev/null
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID
fi

# -S: is a socket file
if [[ -S ${SSH_AUTH_SOCK}  && -n "${SSH_AGENT_PID}" ]] && `ps -p "${SSH_AGENT_PID}" >/dev/null`; then
    motd_ssh_agent=$(while IFS= read -r line; do
        echo -e "${color_purple}ssh-agent${color_nc}   ${color_orange}${line}${color_nc}"
    done) <<< $(ssh-add -l | awk -F' ' '{ print $3 }')
elif [[ -f $SSH_AGENT_ENVFILE ]]; then
    rm -v $SSH_AGENT_ENVFILE
fi


if [[ "${OMZSH_DISABLED}" != "true" ]]; then
    plugins=(
        ansible
        colored-man-pages
        colorize
        docker # auto complete
        git
        #gpg-agent
        nmap
        #pep8
        safe-paste
        urltools # urlencode and urldecode
    )
    if [[ $system = "Darwin" ]]; then
        plugins+=(osx brew)
    fi
    source $ZSH/oh-my-zsh.sh

    # remove unwanted aliases set by ohmzh/plugins
    unalias a 2>/dev/null
else
    # on a system without oh-my-zsh (f.ex. non util/control nodes)
    # set some basic settings
    SAVEHIST=10000
    HISTFILE=~/.zsh_history
fi


fixssh() {
    if [[ -x "$(command -v tmux)" ]]; then
        eval $(tmux show-env | sed -n 's/^\(SSH_[^=]*\)=\(.*\)/export \1="\2"/p')
    else
        echo "fixssh doesnt do anything without tmux"
    fi
}

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
export UPDATE_ZSH_DAYS=31

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"


if [[ "$EDITOR" == "emacs" ]]; then
    source $PRIVATE_DOTFILES/zsh.d/emacs.sh
fi
if [[ -x $(which pmset) ]]; then
    alias speed="sudo pmset -a disablesleep 1"
    alias weed="sudo pmset -a disablesleep 0"
fi

if [[ -x $(which hostname) && "$OSTYPE" != "darwin"* ]]; then
    case $(hostname --fqdn) in
        sensor-*.s21.sudo.is|ber1.sudo.is)
            PROMPT="🌡️   $PROMPT"
            ;;
        *)
    esac
fi


# https://github.com/NixOS/nix/issues/7880#issuecomment-1750399157
if [[ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
