#!/usr/bin/env zsh -xeu

readonly DOTPATH=~/dotfiles

if [ -d "$DOTPATH"] ;then
   echo -e "\e[32;1mUpdating...\e[m"
   git -C "$DOTPATH" pull
   source scripts/linkmaker.zsh
   echo -e "\e[32;1mdotfiles have just been Updated!\e[m"
   
   else
       echo -e "\e[32;1mInstalling...\e[m"
       source scripts/linkmaker.zsh
       wait
       source scripts/setup_starship.zsh
       wait
       source scripts/setup_zplug.zsh
       wait
       source scripts/setup_asdf.zsh
       wait
       if [ -n "$(which wslpath)" ]; then
	   source scripts/linkmaker_win.zsh
	   source scripts/setup_winget.zsh
       fi
       
       echo -e "\e[32;1mdotfiles have just been Installed!\e[m"
fi
