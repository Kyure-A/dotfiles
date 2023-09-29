#!/usr/bin/env zsh -xeu

DOTPATH=~/dotfiles

source $DOTPATH/bin/function.zsh

# . から始まるファイル/ディレクトリ
for file in .??*
do
    [ "$file" = ".cargo" ] && continue
    [ "$file" = ".config" ] && continue
    [ "$file" = ".git" ] && continue
    [ "$file" = ".gitignore" ] && continue
    [ "$file" = ".swp" ] && continue
    [ "$file" = ".tool-versions" ] && continue
	
    ln -snf "$DOTPATH/$file" "$HOME"/"$file" && printf "\033[32mLinked\033[m            $DOTPATH/$file -> $HOME/$file\n" || printf "\033[31mFailed\033[m            $DOTPATH/$file -> $HOME/$file\n"
done

# .config 直下のファイル/ディレクトリ
for file in .config/*
do
    ln -snf "$DOTPATH/$file" "$HOME"/"$file" && printf "\033[32mLinked\033[m            $DOTPATH/$file -> $HOME/$file\n" || printf "\033[31mFailed\033[m            $DOTPATH/$file -> $HOME/$file\n"
done

# .cargo 直下のファイル/ディレクトリ
for file in .cargo/*
do
    ln -snf "$DOTPATH/$file" "$HOME"/"$file" && printf "\033[32mLinked\033[m            $DOTPATH/$file -> $HOME/$file\n" || printf "\033[31mFailed\033[m            $DOTPATH/$file -> $HOME/$file\n"
done

existp "wslpath" &&
    {
	WINPATH="$(wslpath '$(wslvar USERPROFILE)')"
	printf "$WINPATH"
	
	pwsh.exe -Command New-Item -Value '$DOTPATH/.config/pwsh/profile.ps1' -Path '$WINPATH/Documents/PowerShell/' -Name 'Microsoft.PowerShell_profile.ps1' -ItemType SymbolicLink
	printf "\033[32mLinked\033[m            $DOTPATH/.config/pwsh/profile.ps1 -> $WINPATH/Documents/PowerShell/"
	
	pwsh.exe -Command New-Item -Value '$DOTPATH/.config/starship/starship.toml' -Path '$WINPATH/Documents/PowerShell/' -Name 'starship.toml' -ItemType SymbolicLink
	
	
	pwsh.exe -Command New-Item -Value '$DOTPATH/.config/winterm/settings.json' -Path '$WINPATH/AppData/Local/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/' -Name 'settings.json' -ItemType SymbolicLink
	
	## pwsh.exe -ExecutionPolicy Unrestricted -File ./winget.ps1
    }

printf "\033[32mSymbolic Links have just been created.\033[m\n"
