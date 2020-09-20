local return_code="%(?..%{$fg[red]%}%?%{$reset_color%})"

PROMPT='%{$fg[green]%}%m%{$reset_color%} %~ $(git_prompt_info)'

if [ "$NEWLINE" = "true" ]; then
    PROMPT="$PROMPT
"
fi

PROMPT="$PROMPT%{$fg[red]%}%(!.#.$)%{$reset_color%} "


if [ $UID -eq 0 ]; then
    PROMPT="%{$fg[red]%}%n%{$fg[white]%}@$PROMPT"
fi

TZ="Europe/Berlin"
PROMPT="%T $PROMPT"

# PROMPT='%{$fg[$NCOLOR]%}%n%{$fg[green]%}@%m%{$reset_color%} %~ \

PROMPT2='%{$fg[red]%}\ %{$reset_color%}'
RPS1='${return_code}'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg[green]%}%{$fg[yellow]%}<"
ZSH_THEME_GIT_PROMPT_SUFFIX=">%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_CLEAN=""
ZSH_THEME_GIT_PROMPT_DIRTY="*"
