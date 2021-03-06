#
HISTFILE=~/.zhistory
HISTSIZE=100000
SAVEHIST=100000

export EDITOR=emacs
export SUDO_EDITOR=emacs

#------------------------------------------------------------------------------------------------------------------------------

## setopt
setopt appendhistory # 上書きではなく、すぐに履歴を追加する
setopt autocd # ディレクトリパスのみが入力された場合、そこにcdする
setopt autolist # 補完を一覧で表示
setopt automenu # TABで候補の切り替え
setopt correct # 間違いを自動修正する (nyae のやつ)
setopt extendedglob # 正規表現で*を使うことができる
setopt histignorealldups # 新しいコマンドが重複している場合は、古いコマンドを削除する
setopt inc_append_history # 保存したコマンドはすぐに履歴に追加されますが、そうでない場合はシェルが終了したときにのみ追加されます
setopt nobeep # ビープ音を鳴らさない
setopt nocaseglob # 大文字小文字を区別しないグロブイング
setopt nocheckjobs # 終了時に実行中のプロセスに関する警告を出さない
setopt numericglobsort # ファイル名を数字順にソートする
setopt print_eight_bit # 日本語ファイル名を表示可能にする
setopt rcexpandparam # パラメータによる配列の拡張

## Autoload
autoload -U colors
colors
autoload -U predict-on
predict-on
autoload -U zcalc

## Alias
alias cp="cp -i" # Confirm before overwriting something
alias df="df -h" # Human-readable sizes
alias free="free -m" # Show sizes in MB
alias ls="ls -a" #隠しファイルの表示

## Keys
setxkbmap -layout us
bindkey -e # emacs mode
bindkey '^[Oc' forward-word                     
bindkey '^[Od' backward-word                     
bindkey '^[[1;5D' backward-word                    
bindkey '^[[1;5C' forward-word                     
bindkey '^H' backward-kill-word # delete previous word with ctrl+backspace
bindkey '^[[Z' undo # Shift+tab undo last action

## zstyle
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # 大文字小文字を区別しないタブ補完。
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}" # 色付き補完(dirs/files/etcで色が異なる)
zstyle ':completion:*' rehash true # パスに含まれる新しい実行ファイルを自動的に見つける 
zstyle ':completion:*' accept-exact '*(N)' # Speed up completions
zstyle ':completion:*' use-cache on # Speed up completions
zstyle ':completion:*' cache-path ~/.zsh/cache
WORDCHARS=${WORDCHARS//\/[&.;]} # 単語区切り文字の設定

## less
export LESS=-R
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'

#------------------------------------------------------------------------------------------------------------------------------

## zplug
source ~/.zplug/init.zsh
if ! zplug check; then
    zplug install
fi

zplug "b4b4r07/enhancd", use:"init.sh"

zplug "mafredri/zsh-async"

zplug "zsh-users/zsh-autosuggestions"

zplug "zsh-users/zsh-completions"

zplug "zsh-users/zsh-syntax-highlighting"

zplug "zsh-users/zsh-history-substring-search"

zplug "mollifier/anyframe"

zplug "junegunn/fzf-bin", as:command, from:gh-r, rename-to:fzf

zplug "yuki-yano/zeno.zsh"

zplug "asdf-vm/asdf", as:command, dir:"~/.asdf"

if zplug check asdf-vm/asdf; then
    ## asdf
    . $HOME/.asdf/asdf.sh
    # append completions to fpath
    fpath=(~/.asdf/completions $fpath)
    # initialise completions with ZSH's compinit
    autoload -Uz compinit && compinit
fi

zplug load

#------------------------------------------------------------------------------------------------------------------------------

## starship
eval "$(starship init zsh)"
export STARSHIP_CONFIG=~/.config/starship/starship.toml

#------------------------------------------------------------------------------------------------------------------------------

## コマンドがないときにいい感じにないパッケージを入れるか聞く
if [[ -r /usr/share/zsh/functions/command-not-found.zsh ]]; then
    source /usr/share/zsh/functions/command-not-found.zsh
    export PKGFILE_PROMPT_INSTALL_MISSING=1
fi

## Emacs libvterm との連携
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

## bind UP and DOWN arrow keys to history substring search
zmodload zsh/terminfo
