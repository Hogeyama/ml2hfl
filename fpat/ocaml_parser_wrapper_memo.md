# OCaml parser wrapper メモ
ゴチャゴチャしててあまり役に立つ気はしないですが，メモです．必要なくなったら消して構いません．

## OCamlのコードをCompiler-libsをつかってパースする．
ocamlfindでCompiler-libs.commonをロードすると `Parse.use_file` などが使えるようになる．
`Parse.use_file` などについての詳細は，`build/ocaml/parsing` 以下にある．

4.02.0 以降（多分）の `build/ocaml/parsing/parser.mli` を見ると

- implementation
- interface
- toplevel_phrase
- use_file

- parse_core_type
- parse_expression
- parse_pattern


というparser関数があるのがわかる．4.01.0 では後半の4つの代わり(?)に

- any_longident

があった．まだ使わないのでなにが違うかはしらない．

これらはparser.mlyのエントリーポイントを表している．

- プログラムの実装?
- インターフェース?
- インタラクティブ?
- \#use とか #load とか
- ….

`Parser.implementation` は `Parsetree.structure` をつくる  
`Parser.interface` は `Parsetree.signature` をつくる  
`Parser.toplevel_phrase` は `Parsetree.toplevel_phrase` をつくる  
`Parser.use_file` は `Parsetree.toplevel_phrase list` をつくる

とりあえず`Parser.implementation`から始めることにする．つまり`Parsetree.structure`を変換していくことになる．


## ParsetreeをTypedtreeに変換する．
Typemodを使うとParsetreeをTypedtreeに変換できるらしい．
`Parsetree.structure`を変換するには，`Typemod.type_structure`を用いる．
`Typemod.type_structure`の型は

`Env.t -> Parsetree.structure -> Location.t -> Typedtree.structure * Types.signature * Env.t`

なので，引数に`Env.t`や`Location.t`の値も与える必要がある．

これでstructureのTypedtree版と`Types.signature` (`ident.t` が型の名前なのか値の名前なのかなどの属性をもつ）と環境（大体 `Path.t` と`何かの情報`のtupleからなる`Envtbl.t`）が得られる．

MoCHiでは`Types.signature`の情報は捨てている様子．


## TypedtreeをFpat ASTに変換する．
`Typedtree.structure`を地道に展開して変換していく．`Typedtree.structure`から`Typedtree.expression`を取り出してそれについてパターンマッチする．
`Typedtree.structure`は `structure_item list` と `Location.t` と `Env.t` を持つ．`Env.t`は最後の環境らしい．

変数を考えるとむずかしくなるので，まずは整数や小数の定数のみの場合を考える．


### 整数や小数のみの場合
単なる整数や整数の式なら，`Typedtree.structure_item_desc`のパターンマッチで`Tstr_eval` を みれば良いらしい．
`Tstr_eval`は`Typedtree.expression`を持つ．これのレコードのメンバである`Typedtree.expression_desc`についてパターンマッチして，整数などの定数を含む`Texp_constant`をとりだす．`Texp_constant`は`Asttypes.constant`を引数に持つ．
Asttypesの実装は`build/ocaml/parsing`の下にある．
整数や小数はここでFpatのASTに変換する．

次に，加算などの演算に拡張してみる．

### +演算の場合
これも，`Typedtree.structure`のレベルでは，`Typedtree.structure_desc`のパターンマッチで`Tstr_eval`を見ればいい．
`Typedtree.expression`のレベルでは整数と違い，`Typedtree.expression_desc`のパターンマッチで`Texp_constant`ではなく`Texp_apply`をみる．
`Texp_apply`は `(Typedtree.expression * (Typedtree.expression option * Typedtree.optional) list)` を引数にもつ．
ここで，fst部分は関数や演算を表し，snd部分は引数たちを表している．
sndのリストの要素は，とりあえずsnd部分はオプション引数かどうかを表していて，
fst部分の式の有無は，部分適用とかの場合を考えているような気がする．

ので，まずはfstの`Typedtree.expression`を再帰的にさばいてFpatのASTにするために，演算子を取り出せるようにする必要がある．
演算子は`Texp_ident`に含まれるので，`Texp_ident`についての処理を追加する．
`Texp_ident`は `Path.t * Longident.t * Types.value_description` をもつ．
とりあえず`Path.t`からFpatのVarをつくる．

つくったら，FpatのVarから演算子を引っ張りだしてまとめるapplyの関数をつくる．

#### 動かない
これで動くと思ったが，`1+1`を試すと
`Fatal error: exception Typetexp.Error(_, _)` が出た．

toplevelで試すと`Typemod.type_structure`の時点で死んでいるようだった．
`1+1`でも`let _ = 1+1`でも，演算子がはいってるとだめだった

~~~~
# let fs str = Lexing.from_string str;;
# let pt = Parse.implementation (fs "1+1");;
# Typemod.type_structure Env.empty pt Location.none;;
Exception:
Typetexp.Error
 ({Location.loc_start =
    {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 1};
   loc_end = {Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 2};
   loc_ghost = false},
 <abstr>, Typetexp.Unbound_value (Longident.Lident "+")).
~~~~

#### 動いた
環境は`Compmisc.initial_env()`か`Compiler.initial_env()`したものを渡す必要があるらしかった．多分4.01.0以降は`Compmisc`．
適当に`Env.initial`みたいなのを入れていたのでハマった．
あとこの辺は`compiler-libs.bytecomp`あたりが要る（はまった）．

### このあと
いまの道具で簡単に行けそうなのは論理演算を追加した後`if`あたりの拡張かという気がする．
変数をいれたら`fun`とか`let`もいけそうな気がする．パターンマッチとかはどうなってるのかまだちゃんと見ていないのでわからない．

