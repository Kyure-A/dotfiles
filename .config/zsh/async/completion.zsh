zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # 大文字小文字を区別しないタブ補完
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}" # 色付き補完(dirs/files/etcで色が異なる)
zstyle ':completion:*' rehash true # パスに含まれる新しい実行ファイルを自動的に見つける 
zstyle ':completion:*' accept-exact '*(N)' # 補完スピードを上げる
zstyle ':completion:*' use-cache on # 補完スピードを上げる
zstyle ':completion:*' cache-path ~/.zsh/cache
