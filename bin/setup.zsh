#!/usr/bin/env zsh -xeu

# starship

if [ ! type "starship" >/dev/null 2>&1 ]; then
    curl -sS https://starship.rs/install.sh | sh
fi

# zplug

if [ ! type "zplug" >/dev/null 2>&1 ]; then
    curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
fi

if type "zplug" >/dev/null 2>&1; then
    zplug install
fi

# asdf 

## nodejs (for copilot.el)
asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf install nodejs 17.9.1

## deno (for zeno.zsh)
asdf plugin add deno
asdf install deno latest
