#!/bin/zsh 

readonly DOTPATH=~/dotfiles

linkmaker()
{
    for file in .??*
    do
	[ "$file" = ".git" ] && continue
	[ "$file" = ".gitignore" ] && continue
	[ "$file" = ".config/" ] && continue
	[ "$file" = ".config/.config" ] && continue

	ln -snf "$DOTPATH/$file" "$HOME"/"$file"
	echo -e " \u2714  \e[32mLinked!\e[m            $DOTPATH/$file -> $HOME/$file"
    done
}

linkmaker_config()
{
    for file in .config/*
    do
	ln -snf "$DOTPATH/$file" "$HOME"/"$file"
	echo -e " \u2714  \e[32mLinked!\e[m            $DOTPATH/$file -> $HOME/$file"
    done
}

linkmaker
linkmaker_config

echo -e "\e[32;1mSymbolic Links have just been created!\e[m"
