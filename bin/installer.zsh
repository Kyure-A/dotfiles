#!/usr/bin/env zsh -xeu

DOTPATH=~/dotfiles

if [ -d "$DOTPATH" ]; then
    printf "\033[32;1mUpdating...\033[m"
    git -C "$DOTPATH" pull
    source bin/linkmaker.zsh
    if [ type "wslpath" >/dev/null 2>&1 ]; then
	source bin/winlinkmaker.zsh
    fi
    printf "\033[32;1mdotfiles have just been Updated!\033[m"
else
    printf "\033[32;1mInstalling...\033[m"
    git clone https://github.com/Kyure-A/dotfiles/ "$DOTPATH"
    source bin/linkmaker.zsh
    wait
    source bin/setup.zsh
    wait
    if [ type "wslpath" >/dev/null 2>&1 ]; then
	source bin/winlinkmaker.zsh
    fi
    printf "\033[32;1mdotfiles have just been installed!\033[m"
    exec $SHELL -l
fi
