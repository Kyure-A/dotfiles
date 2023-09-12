#!/usr/bin/env zsh -xeu

existp() { type "$1" > /dev/null 2>&1; }

# eask
existp "eask" || npm install -g @emacs-eask/cli

# rtx
existp "rtx" || curl https://rtx.pub/install.sh | sh

# rustup
existp "rustup" || curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# rye
existp "rye" || curl -sSf https://rye-up.com/get | bash

# sheldon
existp "sheldon" || cargo install sheldon

# starship
existp "starship" || curl -sS https://starship.rs/install.sh | sh

# zplug
existp "zplug" || curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh

existp "zplug" && zplug install
