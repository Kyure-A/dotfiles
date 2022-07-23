#!/bin/zsh 

readonly DOTPATH=~/dotfiles

linkmaker()
{
    for file in .??* # . から始まるファイル/ディレクトリ
    do
	[ "$file" = ".git" ] && continue
	[ "$file" = ".gitignore" ] && continue

	ln -snf "$DOTPATH/$file" "$HOME"/"$file"
	echo -e " \u2714  \e[32mLinked!\e[m            $DOTPATH/$file -> $HOME/$file"
    done
}

linkmaker

echo -e "\e[32;1mSymbolic Links have just been created!\e[m"
