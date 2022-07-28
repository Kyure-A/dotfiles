#!/bin/zsh

echo -e "\e[32;1mInstalling..\e[m"

setup_starship()
{
    curl -sS https://starship.rs/install.sh | sh
}

setup_zplug()
{
    curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
    zplug install
}

setup_asdf()
{
    # deno (for copilot.el)
    asdf plugin add nodejs https://github.com/asdf-vm/asdf-nodejs.git
    asdf install nodejs 17.9.1
    
    # deno (for zeno.zsh)
    asdf plugin add deno
    asdf install deno latest
}

setup_winget()
{
    if [ -n "$(which wslpath)" ]; then
	pwsh.exe -ExecutionPolicy Unrestricted -File ./scripts/installer.ps1
    fi
}

setup_starship
setup_zplug
setup_asdf
setup_winget

source scripts/linkmaker.zsh
echo -e "\e[32;1mdotfiles have just been Installed!\e[m"
