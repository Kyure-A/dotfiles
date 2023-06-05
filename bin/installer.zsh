#!/usr/bin/env zsh -xeu

readonly DOTPATH=~/dotfiles

if [ -d "$DOTPATH"] ;then
   echo -e "\e[32;1mUpdating...\e[m"
   git -C "$DOTPATH" pull
   source bin/linkmaker.zsh
   echo -e "\e[32;1mdotfiles have just been Updated!\e[m"
   
   else
       echo -e "\e[32;1mInstalling...\e[m"
       source bin/linkmaker.zsh
       wait
       source bin/setup_starship.zsh
       wait
       source bin/setup_zplug.zsh
       wait
       source bin/setup_asdf.zsh
       wait
       if [ -n "$(which wslpath)" ]; then
	   source bin/linkmaker_win.zsh
	   source bin/setup_winget.zsh
       fi
       
       echo -e "\e[32;1mdotfiles have just been Installed!\e[m"
fi