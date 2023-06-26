#!/usr/bin/env zsh -xeu

curl -sS https://starship.rs/install.sh | sh

curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
zplug install

# asdf 

## nodejs (for copilot.el)
asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf install nodejs 17.9.1
asdf local nodejs 17.9.1

## deno (for zeno.zsh)
asdf plugin add deno
asdf install deno latest
asdf local deno latest