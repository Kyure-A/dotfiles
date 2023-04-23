#!/usr/bin/env zsh -xeu

if [ -n "$(which wslpath)" ]; then
    pwsh.exe -ExecutionPolicy Unrestricted -File ./scripts/winget.ps1
fi
