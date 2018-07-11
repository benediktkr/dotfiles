# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

if [ ! -d $ZSH ]; then
    read -q "REPLY?Do you want to download oh-my-zsh with curlpipe? " -n 1 -r
    if [[ ! $REPLY =~ ^[Yy]$ ]]
    then
        # handle exits from shell or function but don't exit interactive shell
        [[ "$0" = "$BASH_SOURCE" ]] && exit 1 || return 1
    fi
    OMZSH="https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh"
    echo
    echo "moving myself!"
    mv ~/.zshrc ~/.zshrc.tmp
    echo "this will spawn a new shell. pleaes exit this one so i can finish"
    sh -c "$(curl -fsSL $OMZSH)"
    echo "thanks, i'm finishing up now"
    rm ~/.zshrc
    mv ~/.zshrc.tmp ~/.zshrc

fi

# |--  aliases
alias emacs="emacs -nw"
alias ipython="ipython --nosep --no-confirm-exit"
alias less="less -R"
alias dmesg="dmesg --human --color=always -T"
alias stripcomment='grep -v "^#" | grep -v "^[[:space:]]*#" | grep -v "^$"'
alias prettyjson='python -m json.tool'
# from jbs
alias json2yaml="python -c 'import sys, yaml, json; yaml.safe_dump(json.load(sys.stdin), sys.stdout, default_flow_style=False)'"
alias j2y="json2yaml"

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
        alias emacs="/usr/local/Cellar/emacs/25.3/bin/emacs-25.3 -nw"
        ;;
    *.omni.carezen.net)
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
    hh19.sudo.is)
        ZSH_THEME="gianu"
        ;;
    freespace)
        ZSH_THEME="gianu"
        ;;
    burrow)
        alias al="(cd ~/projects/infra/ansible && ansible-playbook -K -i hosts --diff burrow.yml)"
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
        # Refers to the various util servers I use at Care. Eventiually my zsh shell
        # will find itself on other care servers as well.
        ZSH_THEME="afowler"
        alias emacs="emacs -nw --daemon && emacsclient -nw || emacsclient -nw"
        alias changed="less -R -j -1 -p 'changed:' $(ls -1 /var/log/ansible/$USER/ansible-* | tail -1)"
        export PATH=/absible/conf/bin:/ansible/shared/bin:$PATH
        powerup () { eval $(/usr/local/bin/powerup $*) ; }

        # Automatically attach to the tmux session on SSH
        if [[ -z "$TMUX" ]] && [ "$SSH_CONNECTION" != "" ]; then
            tmux attach-session -t ssh_tmux || tmux new-session -s ssh_tmux
        fi

        # Set things that are specific to each environment.
        case $SUBROLE in
            intl-euw|intl-use)
                if [[ $SUBROLE == "intl-euw" ]]; then
                    FLAG=$EU_FLAG
                elif [[ $SUBROLE == "intl-use" ]]; then
                    FLAG=$CA_FLAG
                fi
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
        esac
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
    #PROMPT="$FLAG $PROMPT"
    RPROMPT=$FLAG
fi
