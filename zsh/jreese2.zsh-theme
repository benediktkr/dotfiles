local return_code="%(?..%{$fg[red]%}%?%{$reset_color%})"

# to set the colors in the prompt, you can `export` these vars:
#
# export HOSTNAME_COLOR="green"
# export PROMPT_COLOR="red"
# export PROMPT_HOSTNAME="%m"
#
# to get back to these defaults if you change them in a shell
#
# unset HOSTNAME_COLOR
# unset PROMPT_COLOR
# unset PROMPT_HOSTNAME

# with vars:
PROMPT='%{$fg[${HOSTNAME_COLOR:-green}]%}${PROMPT_HOSTNAME:-%m}%{$reset_color%} %~ $(git_prompt_info)'

# original:
#PROMPT='%{$fg[green]%}%m%{$reset_color%} %~ $(git_prompt_info)'

if [ "$NEWLINE" = "true" ]; then
    PROMPT="$PROMPT
"
fi

PROMPT="$PROMPT%{$fg[${PROMPT_COLOR:-red}]%}%(!.#.$)%{$reset_color%} "


if [ $UID -eq 0 ]; then
    PROMPT="%{$fg[red]%}%n%{$fg[white]%}@$PROMPT"
fi

#TZ="Europe/Berlin"
PROMPT="%D{%H:%M} $PROMPT"

# PROMPT='%{$fg[$NCOLOR]%}%n%{$fg[green]%}@%m%{$reset_color%} %~ \

PROMPT2='%{$fg[red]%}\ %{$reset_color%}'
RPS1='${return_code}'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[green]%}%{$fg[yellow]%}<"
ZSH_THEME_GIT_PROMPT_SUFFIX=">%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="*"
