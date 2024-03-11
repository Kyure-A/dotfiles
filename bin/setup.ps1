# install scoop
Invoke-RestMethod -Uri https://get.scoop.sh | Invoke-Expression
scoop bucket add main
scoop bucket add extras
scoop bucket add nonportable
scoop bucket add ksb https://github.com/Kyure-A/ksb

# install packages
scoop install main/aria2
scoop install main/git
scoop install main/gsudo
scoop install main/starship
scoop install main/winfetch
scoop install extras/archwsl
scoop install extras/discord
scoop install extras/powertoys
scoop install extras/sysinternals
scoop install extras/vivaldi
scoop install nonportable/devtoys-np
scoop install nonportable/google-japanese-input-np
scoop install ksb/keyhac
scoop install ksb/nomeiryoui
scoop install ksb/faithtype-bin
scoop install ksb/stealth

# execute font setting
Auto-Patch-Then-Install # faithtype-bin
nomeiryoui .\setting.ini -set # set noto sans as system font

# install pwsh module
Install-Module WslInterop
Install-Module PSReadLine
