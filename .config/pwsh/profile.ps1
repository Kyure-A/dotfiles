# Import-Module
Import-Module WslInterop
Import-Module PSReadLine

#------------------------------------------------------------------------------------------------------------------------------

# Emacs
Set-PSReadlineOption -EditMode Emacs
Set-PSReadlineKeyHandler -Key Ctrl+d -Function DeleteChar
Import-WslCommand emacs

#------------------------------------------------------------------------------------------------------------------------------

# Starship
Invoke-Expression (&starship init powershell)
$ENV:STARSHIP_CONFIG = "$HOME\Documents\WindowsPowershell\starship.toml"

#------------------------------------------------------------------------------------------------------------------------------

# Auto-suggestion

Set-PSReadLineOption -PredictionSource History

#------------------------------------------------------------------------------------------------------------------------------