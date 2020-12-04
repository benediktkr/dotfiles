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

# |--  aliases (overrides oh-my-zsh plugin aliases)
alias emacs="emacs -nw"
alias ipython="ipython --nosep --no-confirm-exit"
alias less="less -R"
alias dmesg="dmesg --human --color=always -T"
alias stripcomment='grep -v "^#" | grep -v "^[[:space:]]*#" | grep -v "^$"'
alias prettyjson='python -m json.tool'
alias cleangit='git branch | grep -v "master" | xargs git branch -D'
# from jbs
alias json2yaml="python -c 'import sys, yaml, json; yaml.safe_dump(json.load(sys.stdin), sys.stdout, default_flow_style=False)'"
alias j2y="json2yaml"

# !-- python2: enable history
export PYTHONSTARTUP=~/.pystartup
# pipenv wants this
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Set name of the theme to load.
CA_FLAG="🇨🇦 "
EU_FLAG="🇪🇺 "
US_FLAG="🇺🇸"
# Nice ones that I like
#  * jreese
#  * gentoo
#  * alanpeabody (doesnt show full path and color blends with background)

case $HOST in
    BER-Bkristinsson)
        ## My MacBook from Care.com
        ZSH_THEME="robbyrussell"
        alias emacs="/usr/local/bin/emacs -nw"
        export EDITOR=emacs
        ;;
    hogwarts | burrow)
        ZSH_THEME="robbyrussell"
        export EDITOR=emacs
        ;;
    *.omni.internal)
        ROLE="care"
        SUBROLE="omni"
        ;;
    *.euw.intl.carezen.net)
        ROLE="care"
        SUBROLE="intl-euw"
        ;;
    *.use.intl.carezen.net)
        ROLE="care"
        SUBROLE="intl-use"
        ;;
    *.use.dom.carezen.net)
        ROLE="care"
        SUBROLE="dom"
        ;;
    *.mgmt.internal)
        ROLE="care"
        SUBROLE="mgmt"
        ;;
    *.sudo.is)
        ROLE="sudois"
        ;;
    *)
        ZSH_THEME="gianu"
esac

case $ROLE in
    sudois)
        ZSH_CUSTOM="$HOME/.zsh.d/"
        ZSH_THEME="jreese2"

        export EDITOR=emacs

        alias docker='sudo docker'
        alias dockps='docker ps --format "table {{.Names}}\t{{.Image}}\t{{.Status}}"'

        case $HOST in
            mainspace.sudo.is)
                alias nc-occ='docker exec --user www-data nextcloud php occ'
                alias codec='ffprobe -v error -select_streams v:0 -show_entries stream=codec_name -of default=noprint_wrappers=1:nokey=1'
            ;;
            sensor-*)
            alias sibprod='(mkdir -p ~/deadprod && cd ~/deadprod && rsync --exclude="__pycache__" -av ber0:projects/sudoisbot . ) && cd ~/deadprod/sudoisbot && poetry run sudoisbot'
            alias sib="(cd ~ && rsync --exclude="__pycache__" -av ber0:projects/sudoisbot .) && cd ~/sudoisbot && poetry run sudoisbot"
            ;;
        esac

        ;;
    care)
        # Refers to the various util servers I use at Care. Eventiually my zsh shell
        # will find itself on other care servers as well.
        ZSH_THEME="afowler"
        SSH_TMUX="true"
        export BITBUCKET_LOGIN_NAME="benedikt.kristinsson"
        alias ssh="fixssh; ssh"
        alias git="fixssh; git"
        alias emacs="emacs -nw --daemon && emacsclient -nw || emacsclient -nw"
        alias changed="less -R -j -1 -p 'changed:' $(ls -1 /var/log/ansible/$USER/ansible-* | tail -1)"
        alias powerdown='unset AWS_SESSION_TOKEN AWS_CRED_EXPIRATION AWS_SECRET_ACCESS_KEY AWS_ACCESS_KEY_ID AWS_SECURITY_TOKEN'
        export PATH=/ansible/conf/bin:/ansible/shared/bin:$PATH
        powerup () { eval $(/usr/local/bin/powerup $*) ; }
        #ssh() { scp ~/.zshrc $1:~/.zshrc && /usr/bin/ssh $* }

        # Set things that are specific to each environment.
        case $SUBROLE in
            intl-euw|intl-use)
                if [[ $SUBROLE == "intl-euw" ]]; then
                    FLAG=$EU_FLAG
                    region=euw
                elif [[ $SUBROLE == "intl-use" ]]; then
                    FLAG=$CA_FLAG
                    region=use
                fi
                alias groovydiff="sed -i 's/no_log: True/no_log: False/g' ~/ansible/intl/roles/{deploy,webapp,back}/tasks/main.yml; export ANSIBLE_LOG_PATH=/dev/null"
                alias nogroovydiff="git checkout ~/ansible/intl/roles/{deploy,webapp,back}/tasks/main.yml; unset ANSIBLE_LOG_PATH"
                alias prod-mysql='mysql --defaults-file=/usr/local/etc/.my.cnf.useprd.czen'
                alias prod-mysql-rrdb='mysql --defaults-file=/usr/local/etc/.my.cnf.useprd-rrdb.czen'
                alias prod-elb='watch -n 5 elb-check $(elb-check -l | grep prd | grep web)'
                alias stg-elb='watch -n 5 elb-check $(elb-check -l | grep stg | grep web)'
                alias verify='/usr/local/bin/validate-builds ${DEPLOY} ${BACK} ${WEBAPP} ${SOLR}'
                ;;
            dom)
                FLAG=$US_FLAG
                ;;
            omni)
                source /etc/profile.d/vault-env.sh
                ;;
            mgmt)
                intlbronzeenvs=("intl-bronze-euwest1 intl-bronze-useast1")
                intlgoldenvs=("intl-gold-euwest1 intl-gold-useast1 ")
                tfplan () { tfcmd $1 "plan" }
                tfapply() { tfcmd $1 "apply" }
                tfpost() {
                    class=$1
                    shift
                    tfcmd $class "post" $@
                }


                tfcmd () {
                    class=$1
                    shift
                    cmd=$1
                    shift

                    case $class in
                        bronze)
                            envs="${intlbronzeenvs}"
                            ;;
                        gold)
                            envs="${intlgoldenvs}"
                            ;;
                        all)
                            envs="${intlbronzeenvs} ${intlgoldenvs[@]}"
                            ;;
                        *)
                            echo "badarg : '$1'"
                            return
                    esac
                    for env in ${envs[@]}; do
                        current=$(pwd)
                        cd ~/terraform-intl

                        cd ${env}/misc/
                        unset AWS_SESSION_TOKEN
                        unset AWS_CRED_EXPIRATION
                        unset AWS_SECRET_ACCESS_KEY
                        unset AWS_ACCESS_KEY_ID AWS_SECURITY_TOKEN
                        if [[ "${cmd}" == "plan" ]]; then
                            echo "${env}> ts"
                            ts
                        fi
                        tf ${cmd} $@
                        rc=$?
                        echo "${env}> tf ${cmd} ${@}"
                        echo "[${rc}]"
                        cd ../../
                    done
                    cd $current
                }
        esac

        ;;
esac

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
        aws # auto-complete aws commands
        catimg
        colored-man-pages
        colorize
        docker # auto complete
        git
        gpg-agent
        nmap
        pep8
        pip # autocomplete
        pylint # autocomplete
        python # pyfind, pygrep, etc
        rust # autocomplete
        rustup # autocomplete
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

# for loops over "space separated strings" like bash
setopt shwordsplit



# For Tramp in Emacs
if [[ $TERM == "dumb" ]]; then
    # Set a dumber PS1 that tramp will understand
    PS1='> '
    # for tramp to not hang, need the following. cf:
    # http://www.emacswiki.org/emacs/TrampMode
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
fi

if [[ ! -z $FLAG ]]; then
    # If a $FLAG is set, add it to the PROMPT
    #PROMPT="$FLAG $PROMPT"
    RPROMPT=$FLAG
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
