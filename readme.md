## 準備

### ripgrep(rg) install for ubuntu

https://gihyo.jp/admin/serial/01/ubuntu-recipe/0579


以下のどちらかでインストールできる
```
$ sudo apt install ripgrep
$ sudo snap install ripgrep --classic
```

### omnisharpを利用する場合 for windows

1. 以下を実施しomnisharp server をインストールする（一度のみ実施する）  
   M-x: omnisharp-install-server  
   .emacs.d/.cache/omnisharp/server/v1.34.5/... にインストールされる  
2. ソリューション単位にomnisharpを開始する（起動するたびに実行する必要がある）  
   M-x: omnisharp-start-omnisharp-server RET <sln ディレクトリを選択> RET  

## mouse 操作 for only terminal

左ボタン押しながら選択開始、左ボタン離すと選択終了。再度、左ボタン(右ボタンでも可能）押下でクリップボードにコピー  
選択状態で、右ボタンダブルクリックで選択エリア切り取り。  
shift + 右ボタンでペースト。  

## 基本ショートカットコマンド

- C-\ : emacs側のime切り替え（全角文字になって戻らない場合の対応用）
- C-s : インクリメンタルサーチ
- C-M-s : 正規表現インクリメンタルサーチ
- C-/ : アンドゥ
- M-/ : リドウ  
  Undo branch point! が表示されたら C-x u (undo-tree-visualize) 枝(branch)を選択  
  q   : quit  
  p/n : undo <-> redo  
  f/b : 枝移動  
- C-x C-f: ファイル選択＆カーソル行がファイルパスの場合、そのファイルを開けて移動する
- C-x s: 現在ファイル保存
- C-x w: 別名ファイル保存

- M-g g : 指定行にジャンプ
- C-x k : バッファー削除
- C-x 0 : カーソルのあるウィンドウを閉じる
- C-x 1 : 他のウィンドウを全て閉じる
- C-x 2 : ウインドウを上下に分割
- C-x 3 : ウインドウを左右に分割
- C-x o : ウィンドウ間のカーソル移動
- C-x k : ウィンドウ削除

水平
- C-a : 行頭
- M-b : １単語戻る
- C-b : １文字戻る
- C-f : １文字進む
- M-f : １単語進む
- C-e : 行末

垂直
- M-< : ファイル先頭
- M-v : １ページ戻る
- M-[ : １段落戻る
- C-p : １行戻る
- C-n : １行進む
- M-] : 一段落進む
- C-z : １ページ進む
- M-> : ファイル末尾

選択＆削除＆コピー＆ペースト
- C-x @ : 矩形選択開始
- C-@   : 選択開始
- C-RET : 矩形選択開始 + (M-n : 連番付加)
- C-w : マークから現在位置までの範囲を削除する
- M-w : マークから現在位置までの範囲をキルリング（貼り付け用のバッファ）にコピー
- C-y : キルリングの最新の内容を貼り付ける
- M-y  : helm で M-y を実行(キリングバッファー一覧が表示され、選択してペーストできる）

- C-d : カーソルの文字を削除（デリート）
- M-d : カーソルの単語を削除（削除した単語はキルリングに蓄積）
- C-k : カーソルから行末までを削除（削除したものはキルリングに蓄積）

- デフォルトは無効になってる-> C-x, C-u : マークから現在位置までの範囲の文字を大文字にする
- M-u : 単語を大文字にする
- デフォルトは無効になってる-> C-x, C-l : マークから現在位置までの範囲の文字を小文字にする
- M-l : 単語を小文字にする

- M-% : 置換（正規表現)

- M-;     : １行コメント
- C-c ;   : リージョン範囲をコメントアウト

- C-M-\ : 選択範囲をオートインデント

置換時の改行／タブ入力
- C-q C-i: タブ
- C-q C-j: 改行

indent インデント
- （破棄）C-c > : インデント調整(C-c < で逆向き) 
- C-x Tab : f,b(１文字単位), F,B(tab単位) でインデント
- C-x C-b : バッファーリスト表示(counsel-switch-buffer)
  C-k: バッファー削除
- C-c t b : 旧バッファー一覧  
  d : delete  
  x : 実行  

文字コード自動判定が間違ったとき
- C-x RET r (coding system for visited file) で 文字コードを指定しなおすことで対処できる。
- C-x RET f (coding system for saving file) で 文字コードを指定しなおすことで対処できる。


## 関数

### 基本

- M-x string-rectangle : 選択した行の先頭に任意の文字列を挿入することが可能（インデントにも利用できる）
- M-x toggle-case-fold-search : 置換時に大文字小文字区別する／しない 切り替え。デフォルトは”区別しない”になってるらしい。。。
- M-x counsel-imenu : 関数一覧表示

### カレントディレクトリ

- M-x pwd : 現在のカレントディレクトリを確認する
- M-x cd  : 現在のカレントディレクトリを変更する

### diff merge

- M-x: edif-merge : 差分マージ対象ファイルを２ファイル選択、その後、左右（a,b) 、下部(マージ後) の３画面が表示される  
  p & n : 編集行を上下移動  
  a & b : どちらを選択  
  q     : 差分マージセッション終了（マージ済みファイルが謎ハッシュファイル名となって生成される）  
  : ↑が面倒なので、マージ後画面に移動して C-x w で上書きファイルを選択し保存してもいい。  

### gcc コンパイル

- M-x compile : カレントディレクトリでmakeを実行

### neotree

画面左側にソースツリーを表示する
- M-x neotree-toggle 表示／非表示 切り替え  
  neotree にカーソルがある状態
  - C-x {   : ツリー幅を縮める
  - C-x }   : ツリー幅を広める
  - C-c C-c : ルートディレクトリを変更する

### magit

- M-x magit-status : git status を実施  
  status中コマンド
  - s   : stage
  - u   : unstage
  - c c : commit (C-x # : コミットログ記載完了時に実行するコマンド)  

### find-file-in-project

- M-x find-file-in-project

### recentf

- M-x recentf 過去に開いたファイル一覧表示／選択

### ivy

- M-x counsel-gitgrep: git登録内容から検索
- M-x counsel-rg : utf8 文字列検索（超高速）
- M-x counsel-pt : マルチ言語対応検索（現在動作しない）  
  検索後コマンド
  - C-M-m  : チラ見
  - C-c C-o: 検索編集前モード -> "w" キー押下で編集モードになる
  - C-c C-c: 編集結果をファイルに反映
  - C-x d  : ディレクトリ選択

### swiper

- M-x swiper: アクティブバッファの絞り込み検索
- M-x swiper-thing-at-point: 上記に加えて、カーソルにある単語検索

### dumb-jump

https://github.com/jacktasia/dumb-jump
- M-. : 定義ジャンプ
- M-, : ジャンプ元に戻る
- M-? : 参照一覧


### python elpy

```
起動時に以下の問い合わせはYにしておくべき
Automatically install the RPC dependencies from PyPI (needed for completion, autoformatting and documentation)? (Y or n) [please answer Y or n]
```
- M-x : elpy-config : elpy の状況確認画面が表示される
- M-x : pyvenv-activate : プロジェクトルートディレクトリ選択(venvディレクトリを示せばOKのはず）
  linux の場合、source venv/bin/activate しているディレクトリからemacsを起動すれば pyvenv-activate は不要

### vue

- ※ windows版だとnodeのPATHが理解されないらしく正しく動作しない...

準備：  
以下のnodeパッケージをインストールしておかないと動作しない。  
またインストール後、再起動しないと反映しないかもしれない。nodenv rehash でもいいかも。。。  

```
$ npm install -g typescript vls
$ npm install -g typescript-language-server <--- もしかしたらこれも必要
// インストール先を確認するには
$ npm list -g typescript で確認できる
```

初回vueファイルを開くとnode lsp サーバ選択が表示される vls を選択すること。  
次回 emacs 起動では vls 設定は残ったままとなる様子  
- ※ vue ファイルを開くと以下の問い合わせになる場合があるが、無視すればなんとかなるが、本来はjsonファイルを指定するべきで  
  　おそらくプロジェクトルートに配置しておけばいいが、backend, frontend のような複数のプロジェクトが１つのディレクトリに収まっている場合は上手く反応しない。。。

``` text
1    Vetur can't find `tsconfig.json` or `jsconfig.json` in /home/developer/work/cnpj/kokusai/cnpj_fastapi-users-sqlalchemy. 
Learn More
```


プロジェクトルートを指定する方法:
- M-x: lsp-workspace-folders-add  vueプロジェクトルートを指定する(ただしvueファイルを一旦オープンしておく必要がある様子。そのあとリオープンしないとダメっぽい）  
  プロジェクトルートのパスは .emacs.d/.lsp-session-v1 に登録されている様子
- M-x: lsp-workspace-remove-all-folders 全プロジェクト削除
- M-x: lsp 開始

memo:  
init.el に以下のように環境依存するパスが設定されてあるので随時環境に応じて設定する必要がある  
("/home/developer/.anyenv/envs/nodenv/shims/tsc"の箇所)
```lisp
(use-package lsp-mode
  :ensure t
  :hook (vue-mode . lsp)
  :commands lsp
  :config
  (setq lsp-clients-typescript-server "volar")
  (setq lsp-auto-guess-root t)
  (setq lsp-volar-take-over-mode t)
  (setq lsp-volar-typescript-server-path "/home/developer/.anyenv/envs/nodenv/shims/tsc"))  ;; TypeScriptのパスを設定
```

### 色適用 face の調べ方

調べたい文字にカーソルを合わせて
- M-x : describe-char  
  別ペインが表示されるので、「face」欄を調べる
