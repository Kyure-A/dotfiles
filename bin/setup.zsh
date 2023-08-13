#!/usr/bin/env zsh -xeu

existp() { type "$1" > /dev/null 2>&1; }

# asdf
existp "asdf" || git clone https://github.com/asdf-vm/asdf.git ~/.asdf

existp "asdf" &&
    {
	## nodejs
	asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git;
	asdf install nodejs latest;
	
	## deno
	asdf plugin add deno
	asdf install deno latest
    }

# eask
existp "eask" || npm install -g @emacs-eask/cli

# rustup
existp "rustup" || curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# rye
existp "rye" || curl -sSf https://rye-up.com/get | bash

# starship
existp "starship" || curl -sS https://starship.rs/install.sh | sh

# zplug
existp "zplug" || curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh

existp "zplug" && zplug install
