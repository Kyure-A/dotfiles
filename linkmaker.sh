#!/bin/zsh 

readonly DOTPATH=~/dotfiles

for file in .??*
do
    [ "$f" = ".git" ] && continue
    [ "$f" = ".gitignore" ] && continue
    [ "$f" = ".config/" ] && continue

    ln -snfv "$DOTPATH/$file" "$HOME"/"$file"
done

for file in .config/*
do
    ln -snfv "$DOTPATH/$file" "$HOME"/"$file"
done
    
echo -e $'\e[36;5;1mSymbolic links have just been created.\e[m'
