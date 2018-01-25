# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# |--  aliases
alias emacs="emacs -nw"
alias ipython="ipython --nosep --no-confirm-exit"

# Set name of the theme to load.

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
    omni-mgmt-control-0.mgmt.omni.carezen.net)
        # Try to match *.carezen.net and see what happens
        ZSH_THEME="afowler"
        alias emacs="emacs -nw --daemon && emacsclient -nw || emacsclient -nw"
        source /etc/profile.d/vault-env.sh
        powerup () { eval $(/usr/local/bin/powerup $*) ; }
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
