# https://qiita.com/b4b4r07/items/24872cdcbec964ce2178 の install.sh を採用している
#...

DOTPATH=~/.dotfiles

for f in .??*
do
    [ "$f" = ".git" ] && continue

    ln -snfv "$DOTPATH/$f" "$HOME"/"$f"
done
