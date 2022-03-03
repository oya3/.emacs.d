- 準備 -
-- ripgrep(rg) install for ubuntu
# https://gihyo.jp/admin/serial/01/ubuntu-recipe/0579
> $ sudo apt install ripgrep
          or
> $ sudp snap install ripgrep --classic

-- omnisharpを利用する場合 for windows --
(1) 以下を実施しomnisharp server をインストールする（一度のみ実施する）
    M-x: omnisharp-install-server
    .emacs.d/.cache/omnisharp/server/v1.34.5/... にインストールされる
(2) ソリューション単位にomnisharpを開始する（起動するたびに実行する必要がある）
    M-x: omnisharp-start-omnisharp-server RET <sln ディレクトリを選択> RET

- mouse 操作 for only terminal
左ボタン押しながら選択開始、左ボタン離すと選択終了。再度、左ボタン(右ボタンでも可能）押下でクリップボードにコピー
選択状態で、右ボタンダブルクリックで選択エリア切り取り。
shift + 右ボタンでペースト。

- command list
C-\ : emacs側のime切り替え（全角文字になって戻らない場合の対応用）
C-s : インクリメンタルサーチ
C-M-s : 正規表現インクリメンタルサーチ
C-/ : アンドゥ
M-/ : リドウ
      Undo branch point! が表示されたら C-x u (undo-tree-visualize) 枝(branch)を選択
      q   : quit
      p/n : undo <-> redo
      f/b : 枝移動
C-x C-f: ファイル選択＆カーソル行がファイルパスの場合、そのファイルを開けて移動する
C-x s: 現在ファイル保存
C-x w: 別名ファイル保存

M-x string-rectangle : 選択した行の先頭に任意の文字列を挿入することが可能（インデントにも利用できる）

C-c > : インデント調整
C-x C-b : バッファーリスト表示(counsel-switch-buffer)
          C-k: バッファー削除
C-c t b : 旧バッファー一覧
          d : delete
          x : 実行
M-g g : 指定行にジャンプ
counsel-imenu : 関数一覧表示
C-x k : バッファー削除
C-x 0 : カーソルのあるウィンドウを閉じる　
C-x 1 : 他のウィンドウを全て閉じる　　　　
C-x 2 : ウインドウを上下に分割　　　　　　
C-x 3 : ウインドウを左右に分割　　　　　　
C-x o : ウィンドウ間のカーソル移動　　　　
C-x k : ウィンドウ削除　　　　　　　　　　

M-x toggle-case-fold-search : 置換時に大文字小文字区別する／しない 切り替え。デフォルトは”区別しない”になってるらしい。。。

水平
C-a : 行頭
M-b : １単語戻る
C-b : １文字戻る
C-f : １文字進む
M-f : １単語進む
C-e : 行末

垂直
M-< : ファイル先頭
M-v : １ページ戻る
M-[ : １段落戻る
C-p : １行戻る
C-n : １行進む
M-] : 一段落進む
C-z : １ページ進む
M-> : ファイル末尾

選択＆削除＆コピー＆ペースト
C-x @ : 矩形選択開始
C-@   : 選択開始
C-RET : 矩形選択開始 + (M-n : 連番付加)
C-w : マークから現在位置までの範囲を削除する
M-w : マークから現在位置までの範囲をキルリング（貼り付け用のバッファ）にコピー
C-y : キルリングの最新の内容を貼り付ける
M-y  : helm で M-y を実行(キリングバッファー一覧が表示され、選択してペーストできる）

C-d : カーソルの文字を削除（デリート）
M-d : カーソルの単語を削除（削除した単語はキルリングに蓄積）
C-k : カーソルから行末までを削除（削除したものはキルリングに蓄積）

[デフォルトは無効になってる] C-x, C-u : マークから現在位置までの範囲の文字を大文字にする
M-u : 単語を大文字にする
[デフォルトは無効になってる] C-x, C-l : マークから現在位置までの範囲の文字を小文字にする
M-l : 単語を小文字にする

M-% : 置換（正規表現)

M-;     : １行コメント
C-c ;   : リージョン範囲をコメントアウト

C-M-\ : 選択範囲をオートインデント

置換時の改行／タブ入力
C-q C-i: タブ
C-q C-j: 改行

--- カレントディレクトリ ---
M-x pwd : 現在のカレントディレクトリを確認する
M-x cd  : 現在のカレントディレクトリを変更する

--- diff merge ---
M-x: edif-merge : 差分マージ対象ファイルを２ファイル選択、その後、左右（a,b) 、下部(マージ後) の３画面が表示される
                  p & n : 編集行を上下移動
                  a & b : どちらを選択
                  q     : 差分マージセッション終了（マージ済みファイルが謎ハッシュファイル名となって生成される）
                        : ↑が面倒なので、マージ後画面に移動して C-x w で上書きファイルを選択し保存してもいい。

--- gcc コンパイル ---
M-x compile : カレントディレクトリでmakeを実行

--- neotree ---
画面左側にソースツリーを表示する
M-x neotree-toggle 表示／非表示 切り替え
    --- neotree にカーソルがある状態 ---
    C-x {   : ツリー幅を縮める
    C-x }   : ツリー幅を広める
    C-c C-c : ルートディレクトリを変更する

--- magit ---
M-x magit-status : git status を実施
    --- status中コマンド ---
    s   : stage
    u   : unstage
    c c : commit (C-x # : コミットログ記載完了時に実行するコマンド)

--- ivy ---
M-x counsel-rg : utf8 文字列検索（超高速）
M-x counsel-pt : マルチ言語対応検索（現在動作しない）
 検索後コマンド
 C-c C-o: 検索編集前モード -> "w" キー押下で編集モードになる
 C-c C-c: 編集結果をファイルに反映

--- dumb-jump
https://github.com/jacktasia/dumb-jump
M-. : 定義ジャンプ
M-, : ジャンプ元に戻る

文字コード自動判定が間違ったとき
C-x RET r (coding system for visited file) で 文字コードを指定しなおすことで対処できる。
C-x RET f (coding system for saving file) で 文字コードを指定しなおすことで対処できる。

--- elpy ---
;; M-x : elpy-config : elpy の状況確認画面が表示される
;; M-x : pyvenv-activate : プロジェクトルートディレクトリ選択(venvディレクトリを示せばOKのはず）
;;                         linux の場合、source venv/bin/activate しているディレクトリからemacsを起動すれば pyvenv-activate は不要


--- 色適用 face の調べ方 ---
;; 調べたい文字にカーソルを合わせて
;; M-x : describe-char
;;       別ペインが表示されるので、「face」欄を調べる
