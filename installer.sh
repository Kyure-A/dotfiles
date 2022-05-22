#!/bin/zsh

echo -e $'\e[36;5;1mInstall now...\e[m'
curl -sS https://starship.rs/install.sh | sh
curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
zplug install

source linkmaker.sh
echo -e $'\e[36;5;1mdotfiles have just been installed.\e[m'
