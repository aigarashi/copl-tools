# gm から html で整形された規則を生成する

1. `tools/cgi/rules2TeX.scm` にその体系用の整形関数を書く

`gmc` で SExp フォームを出力してそれを評価している

```
make
```
で `games/` 以下に html がはかれる．後は手動コピー!？

2. `games.tex` にTeXマクロを書く

# 体系定義を修正したら

* `checker/` の make をして，`LOCALROOT`へコピー

* `cgi/` で make をして `games/*.html` の生成．
   `games/` まるごと `LOCALROOT` へコピー

* `doc/` で `make rules` をして `rulebook.tex` を編集．pdf の生成
   `rulebook.pdf` を `LOCALROOT` へコピー

* `LOCALROOT/news.scm` の編集

* `LOCALROOT` で

```
cp -r checker games rulebook.pdf news.scm ~/public_htm/CoPL
```
で完了．

