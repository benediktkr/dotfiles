# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

if [ ! -d $ZSH ]; then
    OMZSH="https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh"
    sh -c "$(curl -fsSL $OMZSH)"
fi

# |--  aliases
alias emacs="emacs -nw"
alias ipython="ipython --nosep --no-confirm-exit"
alias less="less -R"
alias dmesg="dmesg --human --color=always"
alias stripcomment='grep -v "^#" | grep -v "^[[:space:]]*#" | grep -v "^$"'


# Set name of the theme to load.
CA_FLAG="🇨🇦 "
EU_FLAG="🇪🇺 "
# Nice ones that I like
#  * jreese
#  * gentoo
#  * alanpeabody (doesnt show full path and color blends with background)

case $HOST in
    BER-Bkristinsson)
        ## My MacBook from Care.com
        ZSH_THEME="robbyrussell"
        alias emacs="/usr/local/Cellar/emacs/25.3/bin/emacs-25.3 -nw"
        ;;
    *.omni.carezen.net)
        # I need a better grouping for the care.com servers to avoid this duplication
        ZSH_THEME="afowler"
        export PATH=$PATH:/ansible/shared/bin
        alias emacs="emacs -nw --daemon && emacsclient -nw || emacsclient -nw"
        source /etc/profile.d/vault-env.sh
        powerup () { eval $(/usr/local/bin/powerup $*) ; }
        ;;
    euwprd-util-1-a.euw.carezen.net)
        FLAG=$EU_FLAG
        ZSH_THEME="afowler"
        alias emacs="emacs -nw --daemon && emacsclient -nw || emacsclient -nw"
        alias prod-elb='watch -n 5 elb-check $(elb-check -l | grep prd | grep web)'
        alias stg-elb='watch -n 5 elb-check $(elb-check -l | grep stg | grep web)'
        alias verify='/usr/local/bin/validate-builds ${DEPLOY} ${BACK} ${WEBAPP} ${SOLR}'
        export PATH=$PATH:/ansible/shared/bin
        ;;
    useprd-util-1-a.use.carezen.net)
        FLAG=$CA_FLAG
        ## SAME AS FOR euw util
        ## Refactor this to be better
        ZSH_THEME="afowler"
        alias emacs="emacs -nw --daemon && emacsclient -nw || emacsclient -nw"
        alias prod-elb='watch -n 5 elb-check $(elb-check -l | grep prd | grep web)'
        alias stg-elb='watch -n 5 elb-check $(elb-check -l | grep stg | grep web)'
        alias verify='/usr/local/bin/validate-builds ${DEPLOY} ${BACK} ${WEBAPP} ${SOLR}'
        export PATH=$PATH:/ansible/shared/bin
        ;;
    dom-use-util-01.use.dom.carezen.net)
        ## SAME AS FOR euw util
        ## Refactor this to be better
        ZSH_THEME="afowler"
        alias emacs="emacs -nw --daemon && emacsclient -nw || emacsclient -nw"
        alias prod-elb='watch -n 5 elb-check $(elb-check -l | grep prd | grep web)'
        alias stg-elb='watch -n 5 elb-check $(elb-check -l | grep stg | grep web)'
        alias verify='/usr/local/bin/validate-builds ${DEPLOY} ${BACK} ${WEBAPP} ${SOLR}'
        export PATH=$PATH:/ansible/shared/bin
        ;;
    freespace)
        ZSH_THEME="gianu"
        ;;
    burrow)
	    ZSH_THEME="jreese"
        ;;
    mathom)
        ZSH_THEME="jreese"
        ;;
    dontpanic)
        ZSH_THEME="evan"
        ;;
    *)
        ZSH_THEME="robbyrussell"
esac

case $ROLE in
    care)
        ZSH_THEME="afowler"
        case $SUBROLE in
            intl)
                echo "intl"
                ;;
            dom)
                echo "dom"
                ;;
            omni)
                echo "omni"
                ;;
        esac
        alias prod-elb='watch -n 5 elb-check $(elb-check -l | grep prd | grep web)'
        alias stg-elb='watch -n 5 elb-check $(elb-check -l | grep stg | grep web)'
        alias verify='/usr/local/bin/validate-builds ${DEPLOY} ${BACK} ${WEBAPP} ${SOLR}'
        export PATH=$PATH:/ansible/shared/bin

        ;;
esac

fixssh() {
    eval $(tmux show-env | sed -n 's/^\(SSH_[^=]*\)=\(.*\)/export \1="\2"/p')
}

hs () {
    pygmentize -l haskell $1 | less -R
}

py () {
    pygmentize -l python $1 | less -R
}

sh () {
    pygmentize -l bash $1 | less -R
}

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

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git screen lein pip python gpg-agent)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=$PATH:/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

export WORKON_HOME=~/.virtualenvs/
#source /home/benedikt/.local/bin/virtualenvwrapper.sh

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

if [[ -n $FLAG ]]; then
    # If a $FLAG is set, add it to the PROMPT
    PROMPT="$FLAG $PROMPT"
fi
