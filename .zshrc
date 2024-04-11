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

#case $HOME in
#    /home/benedikt.kristinsson)
#        ENV="care.com"
#        ;;
#    /home/ben)
#        ENV="care.com"
#        ;;
#    *)
#        ENV="unknown"
#        ;;
#esac

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

export TZ="Europe/Berlin"
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export PATH="$HOME/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/bin:/usr/sbin:/bin:/sbin"
export GPG_TTY=$(tty)

export HATCH_INTERACTIVE=false

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
alias diff='diff --side-by-side'
# from jbs
alias json2yaml="python -c 'import sys, yaml, json; yaml.safe_dump(json.load(sys.stdin), sys.stdout, default_flow_style=False)'"
alias j2y="json2yaml"

alias emacs='~/.local/emacs/bin/emacs -nw'
alias emacsclient='~/.local/emacs/bin/emacsclient -nw'

alias private='git -C ~/.local/share/private'
alias myip='curl https://www.sudo.is/api/myip'

export EDITOR="vim"

# in submodules, .git is a file:
# $ cat ${OMZSH}/.git
# gitdir: ../../.git/modules/zsh/.oh-my-zsh
#if [[ -d "${OMZSH}" && ! -f "${OMZSH}/.git" ]]; then
#    echo "oh-my-zsh submodule needs to be initialized"
#    git -C $DOTFILES submodule update --init  --recursive
$fi

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

    powerup () {
        /usr/local/bin/powerup $* > ${HOME}/.aws/powerup.env
        source ${HOME}/.aws/powerup.env
    }

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


# gpg-agent: this probably isnt needed
# if [[ -f ~/.gnupg/agent-info.env ]]; then
#     eval $(cat ${HOME}/.gnupg/agent-info.env)
# fi


if [[ "${OMZSH_DISABLED}" != "true" ]]; then
    plugins=(
        # emacs # open files via emacsclient everywhere
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


## these are almost never used
#alias stripcomment='grep -v "^#" | grep -v "^[[:space:]]*#" | grep -v "^$"'
#alias prettyjson='python -m json.tool'
#alias cleangit='git branch | grep -v "master" | xargs git branch -D'

fixssh() {
    if [[ -x "$(command -v tmux)" ]]; then
        eval $(tmux show-env | sed -n 's/^\(SSH_[^=]*\)=\(.*\)/export \1="\2"/p')
    else
        echo "fixssh doesnt do anything without tmux"
    fi
}

system=$(uname -s)
case $system in
    Darwin)
        alias speed="sudo pmset -a disablesleep 1"
        alias weed="sudo pmset -a disablesleep 0"
        # defaulting to brew-installed python3
        export PATH="$HOME/Library/Python/3.9/bin:$PATH"
        ;;
esac

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



if [[ $TERM == "dumb" ]]; then
    # if Tramp in Emacs, set a dumber PS1 that tramp will understand
    PS1='> '
    # for tramp to not hang, need the following. cf:
    # http://www.emacswiki.org/emacs/TrampMode
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
else
    # otherwise default to xterm-256color
    export TERM=xterm-256color
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

if [[ -x $(which hostname) && "$OSTYPE" != "darwin"* ]]; then
    case $(hostname --fqdn) in
        sensor-*.s21.sudo.is|ber1.sudo.is)
            PROMPT="🌡️   $PROMPT"
            ;;
        *)
    esac
fi

# if [[ -d "${HOME}/.zsh.d" ]]; then
#     echo "WARNING: ${HOME}/.zsh.d still exists on this system"
# fi


# Automatically attach to the tmux session on SSH
vterm_printf(){
    if [[ -n "$TMUX" ]]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [[ "${TERM%%-*}" == "screen" ]]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
if [[ -x "$(command -v tmux)" &&  "$SSH_TMUX_AUTO_ATTACH" == "true" ]]; then
    if [[ -z "$TMUX" && "$SSH_CONNECTION" != "" ]]; then
        tmux attach-session -t ssh || tmux new-session -s ssh
    fi
fi


# alias sibprod='(mkdir -p ~/deadprod && cd ~/deadprod && rsync --exclude="__pycache__" --exclude="*egg-info" -av ber0:projects/sudoisbot . ) && cd ~/deadprod/sudoisbot && poetry run sudoisbot'
# alias sib="(cd ~ && rsync --exclude="__pycache__" -av ber0:projects/sudoisbot .) && cd ~/sudoisbot && poetry run sudoisbot"
cast() {
    webhook_url=$(cat ~/.hass_webhook_url | tr -d '\n')
    cast_url=$1
    curl -s -i -X POST -d "url=${cast_url}" $webhook_url | head -n 1
}

castnr() {
    url_nr=$1
    cast $(sed -n "${url_nr}p" ~/.cast_urls.txt)
}

# https://github.com/NixOS/nix/issues/7880#issuecomment-1750399157
if [[ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
