#!/usr/bin/env zsh -xeu

readonly DOTPATH=~/dotfiles

if [ -d "$DOTPATH"] ;then
   echo -e "\e[32;1mUpdating...\e[m"
   git -C "$DOTPATH" pull
   source scripts/linkmaker.zsh
   echo -e "\e[32;1mdotfiles have just been Updated!\e[m"
   
   else
       echo -e "\e[32;1mInstalling...\e[m"
       setup_starship
       setup_zplug
       setup_asdf
       setup_winget
       source scripts/linkmaker.zsh
       echo -e "\e[32;1mdotfiles have just been Installed!\e[m"
fi
