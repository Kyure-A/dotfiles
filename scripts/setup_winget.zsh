#!/usr/bin/env zsh -xeu

if [ -n "$(which wslpath)" ]; then
    pwsh.exe -ExecutionPolicy Unrestricted -File ./scripts/installer.ps1
fi
