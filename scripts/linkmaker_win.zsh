#!/usr/bin/env zsh -xeu

readonly DOTPATH=~/dotfiles

if [ -n "$(which wslpath)" ]; then
    
    readonly WINPATH="$(wslpath "$(wslvar USERPROFILE)")"
    
    pwsh.exe New-Item -Value '$DOTPATH/.config/pwsh/profile.ps1' -Path '$WINPATH/Documents/PowerShell/' -Name 'Microsoft.PowerShell_profile.ps1' -ItemType SymbolicLink
    pwsh.exe New-Item -Value '$DOTPATH/.config/starship/starship.toml' -Path '$WINPATH/Documents/PowerShell/' -Name 'starship.toml' -ItemType SymbolicLink
    pwsh.exe New-Item -Value '$DOTPATH/.config/winterm/settings.json' -Path '$WINPATH/AppData/Local/Packages/Microsoft.WindowsTerminal_8wekyb3d8bbwe/LocalState/' -Name 'settings.json' -ItemType SymbolicLink
    
fi
