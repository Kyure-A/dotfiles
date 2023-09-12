#!/usr/bin/env zsh -xeu

DOTPATH=~/dotfiles

for file in .??* # . から始まるファイル/ディレクトリ
do
    [ "$file" = ".cargo" ] && continue
    [ "$file" = ".config" ] && continue
    [ "$file" = ".git" ] && continue
    [ "$file" = ".gitignore" ] && continue
    [ "$file" = ".swp" ] && continue
    [ "$file" = ".tool-versions" ] && continue
	
    ln -snf "$DOTPATH/$file" "$HOME"/"$file"
    echo -e " \u2714  \e[32mLinked!\e[m            $DOTPATH/$file -> $HOME/$file"
done

for file in .config/* # .config 直下のファイル/ディレクトリ
do
    ln -snf "$DOTPATH/$file" "$HOME"/"$file"
    echo -e " \u2714  \e[32mLinked!\e[m            $DOTPATH/$file -> $HOME/$file"
done

for file in .cargo/* # .cargo 直下のファイル/ディレクトリ
do
    ln -snf "$DOTPATH/$file" "$HOME"/"$file"
    echo -e " \u2714  \e[32mLinked!\e[m            $DOTPATH/$file -> $HOME/$file"
done

echo -e "\e[32;1mSymbolic Links have just been created!\e[m"
