

function settitle() {
    # Disable oh-my-zsh default of resetting the terminal title each time the prompt loads
    DISABLE_AUTO_TITLE="true"
    # https://stackoverflow.com/questions/46721797/
    echo -ne "\033]0;$@\007"
}

fixssh() {
    if [[ -x "$(command -v tmux)" ]]; then
        eval $(tmux show-env | sed -n 's/^\(SSH_[^=]*\)=\(.*\)/export \1="\2"/p')
    else
        echo "fixssh doesnt do anything without tmux"
    fi
}


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
