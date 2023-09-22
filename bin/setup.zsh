#!/usr/bin/env zsh -xeu

existp() { type "$1" > /dev/null 2>&1; }

# rye
existp "rye" || curl -sSf https://rye-up.com/get | bash

# rustup
existp "rustup" || curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

## sheldon
existp "sheldon" || cargo install sheldon --locked

## starship
existp "starship" || cargo install starship --locked

## rtx
existp "rtx" || cargo install rtx-cli --locked

### node
existp "node" || { rtx install node@latest; rtx use -g node@latest; }

#### eask
existp "eask" || npm install -g @emacs-eask/cli
