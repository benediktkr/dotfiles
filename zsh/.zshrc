unset MAILCHECK || true

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

if [[ -d $HOME/projects/dotfiles ]] ||
       [[ -d $HOME/dotfiles ]] ||
       [[ -d /srv/dotfiles ]]; then
    USE_OMZ='true'
else
    USE_OMZ='false'
fi

if [[ -d "$HOME/.zsh.d" ]]; then
    ZSH_CUSTOM="$HOME/.zsh.d/"
fi

if [[ -x "$(command -v tmux)" ]]; then
    HAS_TMUX="true"
else
    HAS_TUMX="false"
fi
SSH_TMUX="false"

if [[ $USE_OMZ = 'true' ]] && [[ ! -d $ZSH ]]; then
    read -q "REPLY?Do you want to download oh-my-zsh with curlpipe? " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]
    then
        # handle exits from shell or function but don't exit interactive shell
        [[ "$0" = "$BASH_SOURCE" ]] && exit 1 || return 1
    fi
    OMZSH="https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh"

    curl -fsSL $OMZSH > $HOME/omz-install.sh
    RUNZSH='no' CHSH='no' KEEP_ZSHRC='yes' sh $HOME/omz-install.sh
    rm $HOME/omz-install.sh
    echo "Done setting up oh-my-zsh!"

fi

export TZ="Europe/Berlin"

cast() {
    webhook_url=$(cat ~/.hass_webhook_url | tr -d '\n')
    cast_url=$1
    curl -s -i -X POST -d "url=${cast_url}" $webhook_url | head -n 1
}

castnr() {
    url_nr=$1
    cast $(sed -n "${url_nr}p" ~/.cast_urls.txt)
}

# |--  aliases (overrides oh-my-zsh plugin aliases)
alias emacs="emacs -nw"
alias ipython="ipython --nosep --no-confirm-exit"
alias less="less -R"
alias dmesg="dmesg --human --color=always -T"
alias stripcomment='grep -v "^#" | grep -v "^[[:space:]]*#" | grep -v "^$"'
alias prettyjson='python -m json.tool'
alias cleangit='git branch | grep -v "master" | xargs git branch -D'
alias nomail="echo 'd *' | mail -N"
# from jbs
alias json2yaml="python -c 'import sys, yaml, json; yaml.safe_dump(json.load(sys.stdin), sys.stdout, default_flow_style=False)'"
alias j2y="json2yaml"

# !-- python2: enable history
export PYTHONSTARTUP=~/.pystartup
# pipenv wants this
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# donet telemetry....
export DOTNET_CLI_TELEMETRY_OPTOUT=true

# Set name of the theme to load.
CA_FLAG="🇨🇦 "
EU_FLAG="🇪🇺 "
US_FLAG="🇺🇸"
# Nice ones that I like
#  * jreese
#  * gentoo
#  * alanpeabody (doesnt show full path and color blends with background)

case $(hostname --fqdn) in
    mainframe.sudo.is)
        ROLE="sudois"
        SUBROLE="main"
        ;;
    sensor-*.s21.sudo.is)
        ROLE="sudois"
        SUBROLE="sensor"
        TEMPSENSOR="true"
        ;;
    ber1.sudo.is)
        ROLE="sudois"
        SUBROLE="sensornode"
        ;;
    *.sudo.is)
        ROLE="sudois"
        SUBROLE="server"
        ;;
    *)
        source ~/.config/zsh-roles.sh || ZSH_THEME="gianu"
esac

case $ROLE in
    sudois)
        ZSH_CUSTOM="$HOME/.zsh.d/"
        ZSH_THEME="jreese2"

        export EDITOR=emacs

        alias docker='sudo docker'
        alias dockps='docker ps --format "table {{.Names}}\t{{.Image}}\t{{.Status}}"'

        case $SUBROLE in
            main)
                alias nc-occ='docker exec -it --user www-data nextcloud php occ'
                alias codec='ffprobe -v error -select_streams v:0 -show_entries stream=codec_name -of default=noprint_wrappers=1:nokey=1'
                alias tf='./tf.py'
            ;;
            sensor*)
                #if [[ $HOST = sensor-*.sudo.is ]]; then fi

                alias sibprod='(mkdir -p ~/deadprod && cd ~/deadprod && rsync --exclude="__pycache__" --exclude="*egg-info" -av ber0:projects/sudoisbot . ) && cd ~/deadprod/sudoisbot && poetry run sudoisbot'
                alias sib="(cd ~ && rsync --exclude="__pycache__" -av ber0:projects/sudoisbot .) && cd ~/sudoisbot && poetry run sudoisbot"
            ;;
        esac

        ;;
esac

FLAG=""
NOFLAG="true"

fixssh() {
    if [[ $HAS_TMUX = "true" ]]; then
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
        export PATH="/usr/local/opt/python@3.8/bin:$PATH"
        export PATH="$HOME/Library/Python/3.8/bin:$PATH"
        ;;
esac

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

if [[ $USE_OMZ = 'true' ]]; then
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
    if [[ $ROLE = "care" ]]; then
        unalias a
    fi

else
    # on a system without oh-my-zsh (f.ex. non util/control nodes)
    # set some basic settings
    SAVEHIST=10000
    HISTFILE=~/.zsh_history
fi


export PATH=$HOME/.local/bin:/usr/local/sbin:/usr/local/bin:$PATH
#export PATH="$HOME/.cargo/bin:/:$PATH"

setopt shwordsplit # for loops over "space separated strings" like bash
setopt append_history # append rather then overwrite
setopt extended_history # save timestamp
setopt inc_append_history # add history immediately after typing a command


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
fi

# If a $FLAG is set
if [[ ! -z $FLAG ]]; then
    # and $NOFLAG is NOT set
    if [[ -z $NOFLAG ]]; then
        RPROMPT=$FLAG
    fi
fi
if [[ ! -z $TEMPSENSOR ]]; then
    PROMPT="🌡️   $PROMPT"
fi

if [[ -f "${HOME}/.ssh/agent" ]]; then
    chmod 700 ~/.ssh/agent
    #eval $(cat ~/.ssh/agent) | grep -v "^Agent pid [0-9]*$"
    eval $(cat ~/.ssh/agent)
fi

# Automatically attach to the tmux session on SSH
vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
if [[ -x "$(command -v tmux)" ]] && [ "$SSH_TUMUX" = "true" ]; then
    if [[ -z "$TMUX" ]] && [ "$SSH_CONNECTION" != "" ]; then
        tmux attach-session -t ssh || tmux new-session -s ssh
    fi
fi
