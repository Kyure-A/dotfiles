#
HISTFILE=~/.zhistory
HISTSIZE=100000
SAVEHIST=100000

export EDITOR=emacs
export SUDO_EDITOR=emacs

#------------------------------------------------------------------------------------------------------------------------------

# setopt
setopt appendhistory # 上書きではなく、すぐに履歴を追加する
setopt autocd # ディレクトリパスのみが入力された場合、そこにcdする
setopt autolist # 補完を一覧で表示
setopt automenu # TABで候補の切り替え
setopt correct # 間違いを自動修正する
setopt extendedglob # 正規表現で*を使うことができる
setopt histignorealldups # 新しいコマンドが重複している場合は、古いコマンドを削除する
setopt inc_append_history # 保存したコマンドはすぐに履歴に追加されますが、そうでない場合はシェルが終了したときにのみ追加されます
setopt nobeep # ビープ音を鳴らさない
setopt nocaseglob # 大文字小文字を区別しないグロブイング
setopt nocheckjobs # 終了時に実行中のプロセスに関する警告を出さない
setopt numericglobsort # ファイル名を数字順にソートする
setopt print_eight_bit # 日本語ファイル名を表示可能にする
setopt rcexpandparam # パラメータによる配列の拡張

# Autoload
autoload -U compinit # 自動補完
compinit -d
autoload -U colors
colors
# autoload -U predict-on
# predict-on # predict-on、死ぬほど重くてやめた
autoload -U zcalc

# Alias
alias cp="cp -i" # Confirm before overwriting something
alias df='df -h' # Human-readable sizes
alias free='free -m' # Show sizes in MB

bindkey -e # emacs mode
bindkey '^[Oc' forward-word                                     
bindkey '^[Od' backward-word                                    
bindkey '^[[1;5D' backward-word                                
bindkey '^[[1;5C' forward-word                                  
bindkey '^H' backward-kill-word # delete previous word with ctrl+backspace
bindkey '^[[Z' undo # Shift+tab undo last action

# zstyle
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # 大文字小文字を区別しないタブ補完。
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}" # 色付き補完(dirs/files/etcで色が異なる)
zstyle ':completion:*' rehash true # パスに含まれる新しい実行ファイルを自動的に見つける 
zstyle ':completion:*' accept-exact '*(N)' # Speed up completions
zstyle ':completion:*' use-cache on # Speed up completions
zstyle ':completion:*' cache-path ~/.zsh/cache
WORDCHARS=${WORDCHARS//\/[&.;]} # 単語区切り文字の設定

# less
export LESS=-R
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'

#------------------------------------------------------------------------------------------------------------------------------

# zplug
# $ curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
source ~/.zplug/init.zsh
if ! zplug check; then
    zplug install
fi

zplug "b4b4r07/enhancd", use:"init.sh"

zplug "zsh-users/zsh-autosuggestions"

zplug "zsh-users/zsh-completions"

zplug "zsh-users/zsh-syntax-highlighting"

zplug "mollifier/anyframe"

zplug load

#------------------------------------------------------------------------------------------------------------------------------

# thefuck
eval $(thefuck --alias)
eval $(thefuck --alias FUCK)

# starship
# curl -sS https://starship.rs/install.sh | sh
eval "$(starship init zsh)"

#------------------------------------------------------------------------------------------------------------------------------

# コマンドがないときにいい感じにないパッケージを入れるか聞く
if [[ -r /usr/share/zsh/functions/command-not-found.zsh ]]; then
    source /usr/share/zsh/functions/command-not-found.zsh
    export PKGFILE_PROMPT_INSTALL_MISSING=1
fi

# bind UP and DOWN arrow keys to history substring search
zmodload zsh/terminfo
