#!/usr/bin/env zsh -xeu

DOTPATH=~/dotfiles

if [ -d "$DOTPATH" ]; then
    printf "\033[32;1mUpdating...\033[m\n"
    git -C "$DOTPATH" pull
    source bin/linkmaker.zsh
    printf "\033[32;1mdotfiles have just been Updated!\033[m\n"
else
    printf "\033[32;1mInstalling...\033[m\n"
    git clone https://github.com/Kyure-A/dotfiles/ "$DOTPATH"
    source bin/linkmaker.zsh
    wait
    source bin/setup.zsh
    printf "\033[32;1mdotfiles have just been installed!\033[m\n"
    exec $SHELL -l
fi
