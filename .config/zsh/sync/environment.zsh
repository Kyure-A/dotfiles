HISTFILE=~/.zsh_history
HISTSIZE=10000000
SAVEHIST=10000000

export EDITOR=emacs
export SUDO_EDITOR=emacs
export GIT_MERGE_AUTOEDIT=no

WORDCHARS=${WORDCHARS//\/[&.;]} # 単語区切り文字の設定

export PATH=$HOME/.keg/bin:$PATH
source "$HOME/.rye/env"

export RTX_DATA_DIR=$HOME/.rtx
export RTX_CACHE_DIR=$RTX_DATA_DIR/cache
export STARSHIP_CONFIG=~/.config/starship/starship.toml

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"
