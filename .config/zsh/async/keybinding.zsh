setxkbmap -layout us
bindkey -e # emacs mode
bindkey '^[Oc' forward-word                     
bindkey '^[Od' backward-word                     
bindkey '^[[1;5D' backward-word                    
bindkey '^[[1;5C' forward-word                     
bindkey '^H' backward-kill-word # delete previous word with ctrl+backspace
bindkey '^[[Z' undo # Shift+tab undo last action
