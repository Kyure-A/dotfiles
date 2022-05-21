#!/bin/zsh

echo -e $'\e[36;5;1mInstall now...\e[m'
curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
curl -sS https://starship.rs/install.sh | zsh

source linkmaker.sh
echo -e $'\e[36;5;1mdotfiles have just been installed.\e[m'
