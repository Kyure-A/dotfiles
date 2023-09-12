vterm_printf()
{
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
	# Tell tmux to pass the escape sequences through
	printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
	# GNU screen (screen, screen-256color, screen-256color-bce)
	printf "\eP\e]%s\007\e\\" "$1"
    else
	printf "\e]%s\e\\" "$1"
    fi
}

# vterm-clear-scrollback
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# vterm-buffer-name-string
add-zsh-hook -Uz chpwd () { print -Pn "\e]2;%m:%2~\a" }

# Directory tracking and Prompt tracking
vterm_prompt_end() { vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

vterm_set_directory() { vterm_cmd update-pwd "/-:""$USER""@""$HOSTNAME"":""$PWD/" }
add-zsh-hook -Uz chpwd () { vterm_set_directory }

# Message passing

vterm_cmd()
{
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
	vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
	shift
    done
    vterm_printf "51;E$vterm_elisp"
}

# files opened below the current window.

open_file_below() { vterm_cmd find-file-below "$(realpath "${@:-.}")" }
