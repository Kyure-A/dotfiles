![banner](./assets/banner.png)

<div align="center">
	<h2>
		<img alt="Kyure_A icon" width="18px" src="https://kyure-a.github.io/avatar/src/draw_by_melville.jpg"><a href="https://twitter.com/kyureq">@Kyure_A</a>'s Emacs (for Windows Subsystem for Linux)
	</h2>
</div>

- Shell:
	- <img alt="Windows Icon" width="18px" src="https://raw.githubusercontent.com/github/explore/379d49236d826364be968345e0a085d044108cff/topics/windows/windows.png" /> Windows: 
	<img alt="pwsh icon" width="18px" src="https://learn.microsoft.com/ja-jp/powershell/media/index/ps_black_128.svg"> **pwsh** + <img alt="pwsh icon" width="18px" src="https://upload.wikimedia.org/wikipedia/commons/5/51/Windows_Terminal_logo.svg"> **Windows Terminal** + <img alt="Starship icon" width="18px" src="https://raw.githubusercontent.com/starship/starship/master/media/icon.png"> **Starship**
	
	- <img alt="Linux Icon" width="18px" src="https://raw.githubusercontent.com/github/explore/80688e429a7d4ef2fca1e82350fe8e3517d3494d/topics/linux/linux.png" /> Linux: 
	<img alt="zsh icon" width="18px" src="https://www.zsh.org/favicon.ico"> **zsh** + <img alt="Emacs Icon" width="18px" src="https://raw.githubusercontent.com/github/explore/80688e429a7d4ef2fca1e82350fe8e3517d3494d/topics/emacs/emacs.png" /> **emacs-eat** + 🐚 **Sheldon** + <img alt="Starship icon" width="18px" src="https://raw.githubusercontent.com/starship/starship/master/media/icon.png"> **Starship**

- Editor:

  <img alt="Emacs Icon" width="18px" src="https://raw.githubusercontent.com/github/explore/80688e429a7d4ef2fca1e82350fe8e3517d3494d/topics/emacs/emacs.png" /> **Emacs** +   <img alt="Org icon" width="18px" src="https://upload.wikimedia.org/wikipedia/commons/thumb/a/a6/Org-mode-unicorn.svg/1024px-Org-mode-unicorn.svg.png" /> **org-babel**


![Emacs Screen Shot](./assets/emacs_screenshot.png)

- Runtime manager: 
  - Rust: 
  <img alt="rustup" width="18px" src="https://www.rust-lang.org/logos/rust-logo-64x64.png" /> rustup
  - Python: 
  <img alt="rye" width="18px" src="https://github.com/mitsuhiko/rye/raw/main/docs/static/favicon.svg" /> Rye
  - Other: 
  <img alt="mise" width="18px" src="https://github.com/jdx/rtx/raw/main/docs/logo-dark@2x.png" /> mise (formerly rtx)

## Requirement
- <img alt="Git icon" width="18px" src="https://git-scm.com/favicon.ico"> Git
  
- <img alt="zsh icon" width="18px" src="https://www.zsh.org/favicon.ico"> zsh

- <img alt="Emacs Icon" width="18px" src="https://raw.githubusercontent.com/github/explore/80688e429a7d4ef2fca1e82350fe8e3517d3494d/topics/emacs/emacs.png" /> Emacs

## Installation

To do the installation, all you have to do is copy and paste the only 1 lines of command below and run it.

```zsh
curl -sL https://dotfiles.kyre.moe/bin/installer.zsh | zsh
```

If zsh is not the default, you can run the command below. (This Installation commands are for zsh)

```zsh
chsh -s /bin/zsh
exec $SHELL -l
```

If you want to adapt the settings, restart the shell or run `exec $SHELL -l` .
