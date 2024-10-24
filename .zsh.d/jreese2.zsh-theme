local return_code="%(?..%{$fg[red]%}%?%{$reset_color%})"

# to set the colors in the prompt, you can `export` these vars:
#
# export PROMPT_HOSTNAME="%m"
# export PROMPT_COLOR_HOSTNAME="green"
# export PROMPT_COLOR="red"
# export PROMPT_SHOW_TIME="false"
# export PROMPT_DELIM=" "
#
# to get back to these defaults if you change them in a shell
#
# unset PROMPT_HOSTNAME
# unset PROMPT_COLOR_HOSTNAME
# unset PROMPT_COLOR
# unset PROMPT_SHOW_TIME

# with vars:
PROMPT='%{$fg[${PROMPT_COLOR_HOSTNAME:-green}]%}${PROMPT_HOSTNAME:-%m}%{$reset_color%}${PROMPT_DELIM:- }%~ $(git_prompt_info)'

# Add the path, and ` `/`:` as a delimiter
#PROMPT="${PROMPT}${PROMPT_DELIM:- }%~"

# original:
#PROMPT='%{$fg[green]%}%m%{$reset_color%} %~ $(git_prompt_info)'

if [[ "$PROMPT_NEWLINE" == "true" ]]; then
    #PROMPT="$PROMPT%{$fg[${PROMPT_COLOR:-red}]%} %{$reset_color%}"
    PROMPT="$PROMPT
"
fi

# Adding the `$` (or `#`) marker
PROMPT="${PROMPT}%{$fg[${PROMPT_COLOR:-red}]%}%(!.#.$)%{$reset_color%} "


if [[ $UID == 0 ]]; then
    PROMPT="%{$fg[red]%}%n%{$fg[white]%}@$PROMPT"
fi

#TZ="Europe/Berlin"
if [[ "${PROMPT_SHOW_TIME}" == "true" ]]; then
    PROMPT="%D{%H:%M} $PROMPT"
fi

# PROMPT='%{$fg[$NCOLOR]%}%n%{$fg[green]%}@%m%{$reset_color%} %~ \

PROMPT2='%{$fg[red]%}\ %{$reset_color%}'
RPS1='${return_code}'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[green]%}%{$fg[yellow]%}<"
ZSH_THEME_GIT_PROMPT_SUFFIX=">%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="*"
