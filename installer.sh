#!/bin/zsh

echo -e "\e[32;1mInstalling..\e[m"
curl -sS https://starship.rs/install.sh | sh
curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
zplug install

source linkmaker.sh
echo -e "\e[32;1mdotfiles have just been Installed!\e[m"
