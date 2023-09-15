HISTFILE=~/.zhistory
HISTSIZE=100000
SAVEHIST=100000

export EDITOR=emacs
export SUDO_EDITOR=emacs
export GIT_MERGE_AUTOEDIT=no

WORDCHARS=${WORDCHARS//\/[&.;]} # 単語区切り文字の設定

export PATH=$HOME/.keg/bin:$PATH
source "$HOME/.rye/env"

export RTX_DATA_DIR=$HOME/.rtx
export RTX_CACHE_DIR=$RTX_DATA_DIR/cache
export STARSHIP_CONFIG=~/.config/starship/starship.toml