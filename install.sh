#!/bin/zsh 

readonly DOTPATH=~/dotfiles

for f in .??*
do
    [ "$f" = ".git" ] && continue
    [ "$f" = ".gitignore" ] && continue

    ln -snfv "$DOTPATH/$f" "$HOME"/"$f"
done

echo -e $'\e[36;5;1mSymbolic links have just been created.\e[m'
