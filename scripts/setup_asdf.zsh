#!/usr/bin/env zsh

# nodejs (for copilot.el)
asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
asdf install nodejs 17.9.1
asdf local nodejs 17.9.1

# deno (for zeno.zsh)
asdf plugin add deno
asdf install deno latest
asdf local deno latest
