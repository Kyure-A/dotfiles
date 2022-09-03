#!/usr/bin/env zsh -xeu

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
	
	pwsh.exe New-Item -Value '$DOTPATH/.config/pwsh/profile.ps1' -Path '$WINPATH/Documents/PowerShell/' -Name 'Microsoft.PowerShell_profile.ps1' -ItemType SymbolicLink
	pwsh.exe New-Item -Value '$DOTPATH/.config/starship/starship.toml' -Path '$WINPATH/Documents/PowerShell/' -Name 'starship.toml' -ItemType SymbolicLink
	pwsh.exe New-Item -Value '$DOTPATH/.config/winterm/settings.json' -Path '$WINPATH/AppData/Local/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/' -Name 'settings.json' -ItemType SymbolicLink
	
    fi
}

linkmaker
linkmaker_win

echo -e "\e[32;1mSymbolic Links have just been created!\e[m"
