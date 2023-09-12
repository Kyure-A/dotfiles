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
