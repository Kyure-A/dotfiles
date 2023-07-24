
# zmodload zsh/zprof

HISTFILE=~/.zhistory
HISTSIZE=100000
SAVEHIST=100000

export EDITOR=emacs
export SUDO_EDITOR=emacs

#------------------------------------------------------------------------------------------------------------------------------

## Alias
alias cp="cp -i" # Confirm before overwriting something
alias df="df -h" # Human-readable sizes
alias free="free -m" # Show sizes in MB
alias ls="ls -a" #隠しファイルの表示
alias emacs="emacs -nw"

## Autoload
autoload -U colors
colors
autoload -U predict-on
predict-on
autoload -U zcalc
autoload -U add-zsh-hook
autoload -U compinit
compinit -C

## Keys
setxkbmap -layout us
bindkey -e # emacs mode
bindkey '^[Oc' forward-word                     
bindkey '^[Od' backward-word                     
bindkey '^[[1;5D' backward-word                    
bindkey '^[[1;5C' forward-word                     
bindkey '^H' backward-kill-word # delete previous word with ctrl+backspace
bindkey '^[[Z' undo # Shift+tab undo last action

## less (一応書いてるけどあんまり使ったことない)
export LESS=-R
export LESSCHARSET=utf-8
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'

## setopt
setopt appendhistory # 上書きではなく、すぐに履歴を追加する
setopt autocd # ディレクトリパスのみが入力された場合、そこにcdする
setopt autolist # 補完を一覧で表示
setopt automenu # TABで候補の切り替え
setopt correct # 間違いを自動修正する (nyae のやつ)
setopt extendedglob # 正規表現で*を使うことができる
setopt histignorealldups # 新しいコマンドが重複している場合は、古いコマンドを削除する
setopt inc_append_history # 保存したコマンドはすぐに履歴に追加されるが、そうでない場合はシェルが終了したときにのみ追加される
setopt interactivecomments # コマンドにコメント(# ... )をつけてもエラーが出ないようになる
setopt nobeep # ビープ音を鳴らさない
setopt nocaseglob # 大文字小文字を区別しないグロブイング
setopt nocheckjobs # 終了時に実行中のプロセスに関する警告を出さない
setopt numericglobsort # ファイル名を数字順にソートする
setopt print_eight_bit # 日本語ファイル名を表示可能にする
setopt rcexpandparam # パラメータによる配列の拡張

## zmodload
zmodload zsh/terminfo #履歴の部分文字列検索に<UP> <DOWN> キーを割り当てる

## zstyle
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # 大文字小文字を区別しないタブ補完
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}" # 色付き補完(dirs/files/etcで色が異なる)
zstyle ':completion:*' rehash true # パスに含まれる新しい実行ファイルを自動的に見つける 
zstyle ':completion:*' accept-exact '*(N)' # 補完スピードを上げる
zstyle ':completion:*' use-cache on # 補完スピードを上げる
zstyle ':completion:*' cache-path ~/.zsh/cache
WORDCHARS=${WORDCHARS//\/[&.;]} # 単語区切り文字の設定

#------------------------------------------------------------------------------------------------------------------------------
## PATH
export PATH=$HOME/.keg/bin:$PATH

#------------------------------------------------------------------------------------------------------------------------------

## zplug

source ~/.zplug/init.zsh
if ! zplug check; then
    zplug install
fi

zplug "asdf-vm/asdf", as:command, dir:"~/.asdf"

if zplug check asdf-vm/asdf; then
    ## asdf
    . $HOME/.asdf/asdf.sh
    # append completions to fpath
    fpath=(~/.asdf/completions $fpath)
fi

zplug "mollifier/anyframe"

zplug "b4b4r07/enhancd", use:"init.sh"

zplug "junegunn/fzf", as:command, from:gh-r, rename-to:fzf

zplug "loretoparisi/kakasi", as:command, dir:"~/kakasi"

zplug "yuki-yano/zeno.zsh"

zplug "mafredri/zsh-async"

zplug "zsh-users/zsh-autosuggestions"

zplug "zsh-users/zsh-completions"

zplug "zsh-users/zsh-history-substring-search"

zplug "aoyama-val/zsh-romaji-complete"

if zplug check aoyama-val/zsh-romaji-complete; then
    bindkey "^I" menu-expand-or-complete
fi

zplug "zsh-users/zsh-syntax-highlighting", defer:2
   
zplug load

#------------------------------------------------------------------------------------------------------------------------------

## starship
eval "$(starship init zsh)"
export STARSHIP_CONFIG=~/.config/starship/starship.toml

#------------------------------------------------------------------------------------------------------------------------------

## vterm

### vterm との連携
vterm_printf()
{
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

### vterm-clear-scrollback
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

### vterm-buffer-name-string
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

### Directory tracking and Prompt tracking
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}

vterm_set_directory() {
    vterm_cmd update-pwd "/-:""$USER""@""$HOSTNAME"":""$PWD/"
}

setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
add-zsh-hook -Uz chpwd (){ vterm_set_directory }

### Message passing

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
	vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
	shift
    done
    vterm_printf "51;E$vterm_elisp"
}

### files opened below the current window.

open_file_below() {
    vterm_cmd find-file-below "$(realpath "${@:-.}")"
}

# zprof
