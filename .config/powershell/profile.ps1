# Emacs Keybindings
Import-Module PSReadLine # Install-Module PSReadLine
Set-PSReadlineOption -EditMode Emacs
Set-PSReadlineKeyHandler -Key Ctrl+d -Function DeleteChar

#------------------------------------------------------------------------------------------------------------------------------

# Starship
Invoke-Expression (&starship init powershell)
$ENV:STARSHIP_CONFIG = "$HOME\Documents\WindowsPowershell\starship.toml"

# Auto-suggestion

Set-PSReadLineOption -PredictionSource History