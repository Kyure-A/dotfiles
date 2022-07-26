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

linkmaker_win()
{
    if [ -n "$(which wslpath)" ]; then
	
	readonly WINPATH="$(wslpath "$(wslvar USERPROFILE)")"
	
	# ln -snf "$DOTPATH/$file" "$WINPATH/$file"
	ln -snf "$DOTPATH/.config/pwsh/profile.ps1" "$WINPATH/Documents/PowerShell/Microsoft.PowerShell_profile.ps1"
	ln -snf "$DOTPATH/.config/starship/starship.toml" "$WINPATH/Documents/PowerShell/starship.toml"
	ln -snf "$DOTPATH/.config/winterm/settings.json" "$WINPATH/AppData/Local/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/settings.json"
	
    fi
}

linkmaker
linkmaker_win

echo -e "\e[32;1mSymbolic Links have just been created!\e[m"
