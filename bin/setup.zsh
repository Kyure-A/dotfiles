#!/usr/bin/env zsh -xeu

source $DOTPATH/bin/function.zsh

# rye
existp "rye" && { update_message "rye"; rye self update; } || { install_message "rye"; curl -sSf https://rye-up.com/get | bash;}

# rustup
existp "rustup" && { update_message "rustup"; rustup update; } || { install_message "rustup"; curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh; }

## sheldon
existp "sheldon" && { update_message "sheldon"; cargo update -p sheldon; } || { install_message "sheldon"; cargo install sheldon --locked; }

## delta
existp "git-delta" && { update_message "git-delta"; cargo update -p git-delta; } || { install_message "git-delta"; cargo install git-delta --locked; }

## starship
existp "starship" && { update_message "starship"; cargo update -p starship; } || { install_message "starship"; cargo install starship --locked; }

## rtx
existp "rtx" && { update_message "rtx"; cargo update -p rtx-cli;} || { install_message "rtx"; cargo install rtx-cli --locked; }

## eza
existp "eza" && { update_message "eza"; cargo update -p eza;} || { install_message "eza"; cargo install eza --locked; }

### node
existp "node" || { install_message "node"; rtx install node@latest; rtx use -g node@latest; }

#### eask
existp "eask" || { install_message "eask"; npm install -g @emacs-eask/cli; }
