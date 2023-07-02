#!/usr/bin/env zsh -xeu

DOTPATH=~/dotfiles

if [ -d "$DOTPATH"] ;then
   echo -e "\e[32;1mUpdating...\e[m"
   git -C "$DOTPATH" pull
   source bin/linkmaker.zsh
   echo -e "\e[32;1mdotfiles have just been Updated!\e[m"
   
   else
       echo -e "\e[32;1mInstalling...\e[m"
       source bin/linkmaker.zsh
       wait
       source bin/setup.zsh
       wait
       if [ "$(uname -r)" = *microsoft* ]; then
	   source bin/windows.zsh
       fi
       
       echo -e "\e[32;1mdotfiles have just been Installed!\e[m"
       
       exec $SHELL -l
fi
