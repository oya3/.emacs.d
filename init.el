;; -*- coding: utf-8 -*-
;; command list
;; C-s : インクリメンタルサーチ
;; C-M-s : 正規表現インクリメンタルサーチ
;; C-/ : アンドゥ
;; M-/ : リドウ
;;       Undo branch point! が表示されたら C-x u (undo-tree-visualize) 枝(branch)を選択
;;       q   : quit
;;       p/n : undo <-> redo
;;       f/b : 枝移動
;; C-x C-f: ファイル選択＆カーソル行がファイルパスの場合、そのファイルを開けて移動する
;; C-x s: 現在ファイル保存
;; C-x w: 別名ファイル保存
;;
;; M-x string-rectangle : 選択した行の先頭に任意の文字列を挿入することが可能（インデントにも利用できる）
;;
;; C-c > : インデント調整
;; C-x C-b : バッファーリスト表示
;; C-x C-b : バッファー一覧表示
;; C-c t b : 旧バッファー一覧
;;           d : delete
;;           x : 実行
;; M-g g : 指定行にジャンプ
;; helm imenu : 関数一覧表示
;; C-x k : バッファー削除
;; C-x 0 : カーソルのあるウィンドウを閉じる　
;; C-x 1 : 他のウィンドウを全て閉じる　　　　
;; C-x 2 : ウインドウを上下に分割　　　　　　
;; C-x 3 : ウインドウを左右に分割　　　　　　
;; C-x o : ウィンドウ間のカーソル移動　　　　
;; C-x k : ウィンドウ削除　　　　　　　　　　
;;
;; M-x toggle-case-fold-search : 置換時に大文字小文字区別する／しない 切り替え。デフォルトは”区別しない”になってるらしい。。。
;;
;; 水平
;; C-a : 行頭
;; M-b : １単語戻る
;; C-b : １文字戻る
;; C-f : １文字進む
;; M-f : １単語進む
;; C-e : 行末

;; 垂直
;; M-< : ファイル先頭
;; M-v : １ページ戻る
;; M-[ : １段落戻る
;; C-p : １行戻る
;; C-n : １行進む
;; M-] : 一段落進む
;; C-z : １ページ進む
;; M-> : ファイル末尾

;; 選択＆削除＆コピー＆ペースト
;; C-@ : 選択開始
;; C-SPACE : 選択開始
;; C-RET : 矩形選択開始 + (M-n : 連番付加)
;; C-w : マークから現在位置までの範囲を削除する
;; M-w : マークから現在位置までの範囲をキルリング（貼り付け用のバッファ）にコピー
;; C-y : キルリングの最新の内容を貼り付ける
;; M-y  : helm で M-y を実行(キリングバッファー一覧が表示され、選択してペーストできる）

;; C-d : カーソルの文字を削除（デリート）
;; M-d : カーソルの単語を削除（削除した単語はキルリングに蓄積）
;; C-k : カーソルから行末までを削除（削除したものはキルリングに蓄積）

;; [デフォルトは無効になってる] C-x, C-u : マークから現在位置までの範囲の文字を大文字にする
;; M-u : 単語を大文字にする
;; [デフォルトは無効になってる] C-x, C-l : マークから現在位置までの範囲の文字を小文字にする
;; M-l : 単語を小文字にする

;; M-% : 置換（正規表現)

;; C-c ; : リージョン範囲をコメントアウト
;; C-;   : １行コメント

;; C-M-\ : 選択範囲をオートインデント

;; 置換時の改行／タブ入力
;; C-q C-i: タブ
;; C-q C-j: 改行
;;
;; --- カレントディレクトリ ---
;; M-x pwd : 現在のカレントディレクトリを確認する
;; M-x cd  : 現在のカレントディレクトリを変更する
;;
;; --- diff merge ---
;; M-x: edif-merge : 差分マージ対象ファイルを２ファイル選択、その後、左右（a,b) 、下部(マージ後) の３画面が表示される
;;                   p & n : 編集行を上下移動
;;                   a & b : どちらを選択
;;                   q     : 差分マージセッション終了（マージ済みファイルが謎ハッシュファイル名となって生成される）
;;                         : ↑が面倒なので、マージ後画面に移動して C-x w で上書きファイルを選択し保存してもいい。
;; 
;; --- gcc コンパイル ---
;; M-x compile : カレントディレクトリでmakeを実行
;; 
;; --- neotree ---
;; 画面左側にソースツリーを表示する
;; M-x neotree-toggle 表示／非表示 切り替え
;;     --- neotree にカーソルがある状態 ---
;;     C-x { : ツリー幅を縮める
;;     C-x } : ツリー幅を広める
;;
;; --- magit ---
;; M-x magit-status : git status を実施
;;     --- status中コマンド ---
;;     s   : stage
;;     u   : unstage
;;     c c : commit (C-x # : コミットログ記載完了時に実行するコマンド)
;; --- helm ---
;; * do リアルタイムで検索結果が表示される
;; * do の場合、日本語検索がうまく動作しない。日本語検索の場合は、ag を利用する。
;; M-i :いつでもどこでもカレントバッファーをhelm検索
;; M-x helm-(do-)ag              : カレントディレクトリ配下のグレップ検索
;;     helm-(do-)ag-project-root : .svn, .git, .hg プロジェクト配下のグレップ検索
;;     helm-resume               : 直前の検索結果を呼び出す
;;     helm-ls-git-ls            : .git 管理下のファイル一覧
;;     cd                        : カレントディレクトリ変更
;;     --- 検索結果表示中 ---
;;     改行                      : 該当ファイルにジャンプ
;;     C-j(C-z)                  : 該当ファイルを表示(ちょい見)
;;     C-c C-e                   : 編集開始
;;     C-c C-c                   : 編集内容を各ファイルに保存
;;     C-c C-k                   : 編集内容を破棄
;;     C-c i                     : 関数／定数 一覧   'helm-imenu
;;     C-c r                     : 検索結果再表示    'helm-resume
;;     C-c p                     : 検索元に戻る      'helm-ag-pop-stack
;;     C-x b                     : バッファー一覧表示 'helm-buffers-list
;;                               :                    C-@ : マーク＆アンマーク
;;                               :                    M-D : kill buffers
;;     C-l                       : 一階層上がる
;;     helm-ag-pop-stack         : ジャンプ前に戻る
;; 参考サイト
;; * 来年も使いたいelispランキング2013
;;   http://qiita.com/l3msh0/items/97909d6e2c92af3acc00#1-9
;;
;; 文字コード自動判定が間違ったとき
;; C-x RET r (coding system for visited file) で 文字コードを指定しなおすことで対処できる。
;; C-x RET f (coding system for saving file) で 文字コードを指定しなおすことで対処できる。
;;
;; windows 専用設定だけど、どうも26.1では動かない気がする。。。
;; SETX /M ALTERNATE_EDITOR "C:\tools\emacs\bin\runemacs.exe"
;; SETX /M EMACS_SERVER_FILE "C:\Users\oya\.emacs.d\server\main_server"
;;
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; OS 確認用デバッグメッセージ
;; - Emacsの設定ファイルをOSの判定をして共有する方法｜system-type
;;   https://nagayasu-shinya.com/emacs-system-type/
(message "OS is %s." system-type)
(message "HOME is %s." (getenv "HOME"))
(message "PATH is %s." (getenv "PATH"))

(setenv "LC_MESSAGES" "C")

(add-to-list 'load-path "~/.emacs.d/elisp")

;; cask 設定
;; for mac
(when (eq system-type 'darwin)
  (require 'cask)
  (define-key global-map [?¥] [?\\])  ;; ¥の代わりにバックスラッシュを入力する
  )
;; for windows and linux
(when (not (eq system-type 'darwin))
  (require 'cask "~/.cask/cask.el")
  )
(cask-initialize)

; server start for emacs-client
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t)
(setq server-socket-dir "~/.emacs.d")
(unless (server-running-p)
  (server-start)
  )

;; ;; 日本語環境(言語設定)
(set-language-environment "Japanese")
;; (set-default-coding-systems 'utf-8-unix) ; デフォルトの文字コード
;; (prefer-coding-system 'utf-8-unix)
;; ----------------------------------------------------------------------
;; ;; 上の設定を euc-jp 環境の場合、以下に変更するといいことがあるかも
;; ;; --->
;; (set-default-coding-systems 'euc-jp)
;; (prefer-coding-system 'euc-jp)
;; ;; <---

;; PATH 追加
;; - Windows で Linuxコマンド を使える！ msys2！
;;   https://nagayasu-shinya.com/emacs-msys2-path/
;; (message "oya debug: system-type: %s" system-type)
(when (eq system-type 'windows-nt)
  (set-default-coding-systems 'utf-8-unix) ; デフォルトの文字コード
  (prefer-coding-system 'utf-8-unix)

  ;; MSYS2 のコマンドを使えるようにする.
  (setenv "PATH"
	  (concat
	   ;; 下記の行に MSYS2 のコマンドの実行可能ファイルがある場所を設定してください. スラッシュが2つ連続することに注意！
	   ;; 区切り文字はセミコロン
	   "C:\\msys64\\usr\\bin;"
	   (concat (getenv "HOME") "\\.emacs.d\\bin;")
	   (getenv "PATH")))
  (setq exec-path (parse-colon-path (getenv "PATH"))) ;; 実行パスも同じにする
  ;; windowsの場合のみ
  (setq-default buffer-file-coding-system 'japanese-cp932-dos)
  ;; 以下を有効にすると"◎"の特殊な文字でemacsが固まってしまうので削除。文字化け対応は別途検討が必要
  ;; ;; 環境依存文字 文字化け対応
  ;; (set-charset-priority 'ascii 'japanese-jisx0208 'latin-jisx0201
  ;; 			'katakana-jisx0201 'iso-8859-1 'cp1252 'unicode)
  ;; (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)
  )

;; (set-default-coding-systems 'japanese-cp932-dos)
;; (set-buffer-file-coding-system 'utf-8)

;; (setq default-process-coding-system '(utf-8-dos . cp932))
;; (set-language-environment "Japanese")
;; (prefer-coding-system 'utf-8)
;; (set-file-name-coding-system 'cp932)
;; (set-keyboard-coding-system 'cp932)
;; (set-terminal-coding-system 'cp932)
;; (set-buffer-file-coding-system 'cp932)

;; Proxy の設定
;; (setq url-proxy-services '(("http" . "Proxy:Port")))

;; 【Emacs】Mac/Win及びGUI/CUIで初期設定ファイルを兼用する
;;  環境確認方法がわかる
;;  - https://www.yokoweb.net/2016/12/25/emacs-mac-win-select/#toc3
;; ;; mac でかつターミナルの場合のみ、alt と command 切り替え
;; (when (eq system-type 'darwin)
;;   (if (not window-system) (progn
;;     ;; ←CUI用設定を、ここに記述
;;     ;; CommandとOptionを入れ替える
;;     (setq mac-command-modifier 'meta)
;;     (setq mac-option-modifier 'alt)
;;     (setq mac-command-modifier 'alt)
;;     (setq mac-option-modifier 'meta)
;;     (setq ns-command-modifier (quote meta))
;;     (setq ns-alternate-modifier (quote super))
;;   ))
;; )

;; 起動時のディレクトリの変更
;; (setq default-directory "~/")

;;(set-default-coding-systems 'utf-8) ; デフォルトの文字コード
;;(prefer-coding-system 'utf-8)
;;(set-default 'buffer-file-coding-system 'japanese-cp932-dos)

;; 行番号はシステムを使う
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
;;----------------------------------------------------------
;; 行番号表示
;; (global-linum-mode t)
;; (setq linum-format "%d ")
;; (set-face-attribute 'linum nil
;; 		    :foreground "#a9a9a9"
;; 		    :height 0.9)

;;----------------------------------------------------------
;; undo redo 設定
;; http://d.hatena.ne.jp/khiker/20100123/undo_tree
;; (require 'undo-tree)
;; (global-undo-tree-mode)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;;----------------------------------------------------------
;; indent guide 設定
(require 'indent-guide)
(indent-guide-global-mode)

;;----------------------------------------------------------
;; indent 設定
(require 'indent-tools)
(global-set-key (kbd "C-c >") 'indent-tools-hydra/body)
(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map (kbd "C-c >") 'indent-tools-hydra/body))
          )

;;----------------------------------------------------------
;; mouse 設定
(if window-system (progn
		    (xterm-mouse-mode t)
		    (mouse-wheel-mode t)
		    (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 3)))
		    (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up   3)))
		    ))

;;----------------------------------------------------------
;; フォント設定
;; - emacs 24.4 でのちょっと賢いフォント設定
;;   http://blog.livedoor.jp/tek_nishi/archives/8590439.html
;; - Emacs 24.3/24.4 on Mac のフォント設定Add Starmomo-samuraipogin
;;   http://d.hatena.ne.jp/kazu-yamamoto/20140625/1403674172
;; MAC用フォント設定
;; (when (memq window-system '(mac ns))
(when (eq system-type 'darwin)
  (when (not (eq window-system nil))
    ;; (set-face-attribute 'default nil :family "Menlo" :height 140)
    (set-face-attribute 'default nil :family "Monaco" :height 120)
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208
                     (font-spec :family "Hiragino Kaku Gothic ProN"))
    (add-to-list 'face-font-rescale-alist
                 '(".*Hiragino Kaku Gothic ProN.*" . 1.2))
    )
  )

;; 等幅フォント確認用
;; 　　　　　
;; 0123456789
;; ０１２３４

;; Windows 用のフォント設定
(when (eq system-type 'windows-nt)
  ;; (set-face-font 'default "MeiryoKe_Gothic")
  ;; (set-face-font 'default "ＭＳ ゴシック-12")
  ;; (set-face-font 'default "Ricty Diminished-12")
  (set-face-font 'default "Ricty Diminished-12")
  ;;
  ;; (set-face-attribute 'default nil :family "Inconsolata" :height 110)
  ;; ;; (set-face-attribute 'default nil :family "Consolas" :height 104)
  ;; (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "MeiryoKe_Console"))
  ;; (setq face-font-rescale-alist '(("MeiryoKe_Console" . 1.08)))
  ;; (setq use-default-font-for-symbols nil)
  ;;
  ;; ;; デフォルトはASCII用のフォントでなければダメっぽい。
  ;; (set-face-attribute 'default nil :family "Inconsolata" :height 120)
  ;; ;; ASCII以外のUnicodeコードポイント全部を一括で設定する。他国語を使用する人は細かく指定した方が良いかも。
  ;; (set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "MS Gothic"))
  ;; ;; 記号をデフォルトのフォントにしない。(for Emacs 25.2)
  ;; (setq use-default-font-for-symbols nil)
  )

;; linux
(when (eq system-type 'gnu/linux)
  (set-face-font 'default "ricty-12")
  )

;; cygwin
(when (eq system-type 'cygwin)
  ;; (set-face-font 'default "MeiryoKe_Gothic")
  ;; (set-face-font 'default "ＭＳ ゴシック-12")
  ;; (set-face-font 'default "Ricty Diminished-12")
  (set-face-font 'default "Ricty Diminished-12")
  (setenv "PATH"
	  (concat
	   (concat (getenv "HOME") "/.emacs.d/bin:")
	   (getenv "PATH")))
  (setq exec-path (parse-colon-path (getenv "PATH"))) ;; 実行パスも同じにする
  ;; windowsの場合のみ
  (setq-default buffer-file-coding-system 'japanese-cp932-dos)
  )

;; 画面が1980x1080場合、[110]
;; 画面が1980x1080で1.2拡大している場合、[100]
;; デフォルトのフォントサイズを指定（10単位らしい）
;; - emacsでフォントサイズを調整する
;;   http://haruo31.underthetree.jp/2014/09/11/emacs%E3%81%A7%E3%83%95%E3%82%A9%E3%83%B3%E3%83%88%E3%82%B5%E3%82%A4%E3%82%BA%E3%82%92%E8%AA%BF%E6%95%B4%E3%81%99%E3%82%8B/
;; (set-face-attribute 'default nil :height 100)

;; font size zoom
;; - Ctrl+マウススクロールでズーム
;;   http://www.geocities.jp/tamiyagi2/emacs.html#fontsize
(if (and (>= emacs-major-version 23) (window-system))
    (progn
      ;; (global-set-key (kbd "<C-mouse-2>") 'text-scale-set)
      (global-set-key
       (vector (list 'control mouse-wheel-down-event))
       'text-scale-increase)
      (global-set-key
       (vector (list 'control mouse-wheel-up-event))
       'text-scale-decrease)))

;; ホイールでスクロールする行数を設定
;; - マウスホイールでのスクロールを超快適にする3つの変数設定
;;   http://emacs.rubikitch.com/mouse-wheel/
(setq  mouse-wheel-scroll-amount '(2 ((shift) . 5) ((control)))
       ;; 速度を無視する
       mouse-wheel-progressive-speed nil)
;; スクロール時にカーソル位置を一定にする
(setq scroll-preserve-screen-position 'always)

;; windows IME 設定
;;(when (eq system-type 'windows-nt)
  ;; (when (locate-library "w32-ime")
  ;;   (progn
  ;;     (w32-ime-initialize)
  ;;     (setq default-input-method "W32-IME")
  ;;     (setq-default w32-ime-mode-line-state-indicator "[--]")
  ;;     (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
  ;;     ))
;;  )

;; 環境別ファイル読み込み(windows=init_w32.ini,mac=init_mac.ini)
;; (load (locate-user-emacs-file (concat "init_" (prin1-to-string window-system) ".el") ) )

;;----------------------------------------------------------
;; 基本設定
;; dracula theme
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; アクティブバッファー強調表示
(if window-system (progn
		    (when (require 'dimmer nil t)
		      (setq dimmer-fraction 0.6)
		      (setq dimmer-exclusion-regexp "^\\*helm\\|^ \\*Minibuf\\|^\\*Calendar") 
		      (dimmer-mode 1))
		    (with-eval-after-load "dimmer"
		      (defun dimmer-off ()
			(dimmer-mode -1)
			(dimmer-process-all))
		      (defun dimmer-on ()
			(dimmer-mode 1)
			(dimmer-process-all))
		      (add-hook 'focus-out-hook #'dimmer-off)
		      (add-hook 'focus-in-hook #'dimmer-on))
		    ))

;; desktop-save-mode 終了時のフレーム状態を保存
(if window-system (progn
		    (desktop-save-mode 1)
		    ))

;; 自動バックアップ無効
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 複数ウィンドウを開かないようにする
(setq ns-pop-up-frames nil)

;; 起動メッセージを表示しない
(setq inhibit-startup-message t)

;; *scratch* buffer のメッセージを消す。
(setq initial-scratch-message t)

;; ツールバーを非表示
(tool-bar-mode 0)

;; キャレットが '(' や ')' の直後にある場合に対応する括弧を強調
(show-paren-mode t)
(setq show-paren-delay 0)

;; ウィンドウ幅で折り返さない設定
;; 通常のウィンドウ用の設定
(setq-default truncate-lines t)
;; ウィンドウを左右に分割したとき用の設定
(setq-default truncate-partial-width-windows t)

;; ウィンドウ移動
;; shift ↑↓←→
(setq windmove-wrap-around t)
(windmove-default-keybindings)

;; ;; 勝手にウィンドウを分割するのをやめる
;; (setq split-width-threshold nil)
;; (setq split-height-threshold nil)

;; タイトルバーに編集中のファイルのパス名を表示
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;; neotree 設定
;; 
;; - neotree-toggle toggle/hide NeoTree window
;; - neotree-stretch-toggle Maximize / Minimize
;; - n next line ， p previous line。
;; - SPC or RET or TAB Open current item if it is a file. Fold/Unfold current item if it is a directory.
;; - U Go up a directory
;; - g Refresh
;; - A Maximize/Minimize the NeoTree Window
;; - H Toggle display hidden files
;; - C-c C-n Create a file or create a directory if filename ends with a ‘/’
;; - C-c C-d Delete a file or a directory.
;; - C-c C-r Rename a file or a directory.
;; - C-c C-c Change the root directory.
;; - C-c C-p Copy a file or a directory.
;;
;; C-x }, C-x { でwindowサイズを変更できるよにする
(setq neo-window-fixed-size nil)
;; neotreeでファイルを新規作成した場合のそのファイルを開く
(setq neo-create-file-auto-open t)
;; delete-other-window で neotree ウィンドウを消さない
(setq neo-persist-show t)

;; タブの挙動(一般的なタブの挙動） ※これを無効にすると、tab-mark が表示されなくなる
(setq indent-line-function 'tab-to-tab-stop)

;; バックデリート有効
(global-set-key "\C-h" 'delete-backward-char)

;;----------------------------------------------------------
;; 矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; 変なキーバインド禁止

;;----------------------------------------------------------
;; tabbar
;; tabbar上でウィールマウスボタンで削除
(defun my-tabbar-buffer-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (if tabbar--buffer-show-groups
      (let* ((tabset (tabbar-tab-tabset tab))
             (tab (tabbar-selected-tab tabset)))
        (format "mouse-1: switch to buffer %S in group [%s]"
                (buffer-name (tabbar-tab-value tab)) tabset))
    (format "\
mouse-1: switch to buffer %S\n\
mouse-2: kill this buffer\n\
mouse-3: delete other windows"
            (buffer-name (tabbar-tab-value tab)))))

;;----------------------------------------------------------
;; tabar上でマウスで選択
(defun my-tabbar-buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-2)
      (with-current-buffer buffer
        (kill-buffer)))
     ((eq mouse-button 'mouse-3)
      (delete-other-windows))
     (t
      (switch-to-buffer buffer)))
    ;; Don't show groups.
    (tabbar-buffer-show-groups nil)))

;; tabbarはwindowシステムの場合のみ
(if window-system (progn
		    ;; tabbar有効化
		    (tabbar-mode 1)
		    ;; タブ上でマウスホイール操作無効
		    (tabbar-mwheel-mode -1)
		    ;; 画像を使わないことで軽量化する
		    (setq tabbar-use-images nil)
		    ;; タブグループを１つにする（これしないと読み込み度にウィンドウが分割される？）
		    (setq tabbar-buffer-groups-function nil)
		    ;; 独自マウス操作
		    (setq tabbar-help-on-tab-function 'my-tabbar-buffer-help-on-tab)
		    (setq tabbar-select-tab-function 'my-tabbar-buffer-select-tab)
		    ;; バッファ切り替え
		    (global-set-key [\M-right] 'tabbar-forward-tab)
		    (global-set-key [\M-left] 'tabbar-backward-tab)
		    ))

;;----------------------------------------------------------
;; マウス マークセット
(setq mouse-drag-copy-region t)

;;----------------------------------------------------------
;; リージョン コメント＆アンコメント
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
;; １行コメント＆アンコメント
(defun one-line-comment ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (end-of-line)
    (comment-or-uncomment-region (region-beginning) (region-end))))
(global-set-key (kbd "C-;") 'one-line-comment)

;;----------------------------------------------------------
;; カラム数表示
;; - カラムがゼロ始まりなのが奇妙。rubocopと考え違うらしい： https://github.com/bbatsov/rubocop/issues/276
(column-number-mode t)

;;----------------------------------------------------------
;; カーソル位置(現在行)をハイライト
(defface hlline-face3
  '((((class color)
      (background dark))
     (:background "gray20"))
    ;; :underline "#404040"))
    (((class color)
      (background light))
     (:background "ForestGreen"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face3)
(global-hl-line-mode t)
;; 以下のハイライトを利用するとカーソル移動が早くなるらしいけど。。。まったく一緒だったので使わない
;; (require 'hl-line)
;; ;;; hl-lineを無効にするメジャーモードを指定する
;; (defvar global-hl-line-timer-exclude-modes '(todotxt-mode))
;; (defun global-hl-line-timer-function ()
;;   (unless (memq major-mode global-hl-line-timer-exclude-modes)
;;     (global-hl-line-unhighlight-all)
;;     (let ((global-hl-line-mode t))
;;       (global-hl-line-highlight))))
;; (setq global-hl-line-timer
;;       (run-with-idle-timer 0.05 t 'global-hl-line-timer-function))
;; ;; (cancel-timer global-hl-line-timer)

;; 対応する括弧をハイライト
(show-paren-mode t)
;; (setq show-paren-style 'mixed) ;; 括弧のハイライトの設定。
;; (transient-mark-mode t)        ;; 選択範囲をハイライト

;; CommandとOptionを入れ替える
;; (when (equal system-type 'darwin)
;; (when (memq window-system '(mac ns))
;;   (setq mac-option-modifier 'meta)
;;   (setq mac-command-modifier 'super)
;;   )

;; ;; GUI なら背景を半透明化
;; (if window-system (progn
;;                     ;; 画面Color(背景)
;;                     (set-background-color "#1f1f1f")
;;                     (set-foreground-color "LightGray")
;;                     (set-cursor-color "Gray")
;;                     ;; ウィンドウを透明化
;;                     (add-to-list 'default-frame-alist '(alpha . 90))
;;                     ))

;;----------------------------------------------------------
;; 空白関係
(require 'whitespace)
(setq whitespace-style '(
                         face           ; faceで可視化
;                         trailing       ; 行末
                         tabs           ; タブ
                         tab-mark       ;
                         spaces         ; スペース
                         space-mark     ; 表示のマッピング
;                         empty
                         newline        ; 改行
                         newline-mark
                         ))

(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        ;; (space-mark ?\u0020 [?\xB7])  ; 半角スペース
        ;; (newline-mark ?\n   [?\u21B5 ?\n]) ; 改行記号
        (newline-mark ?\n   [?$ ?\n]) ; 改行記号
        ;; (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]) ; タブマーク
        (tab-mark ?\t [?> ?\t]) ; タブマーク
        )
      )

;;----------------------------------------------------------
;; スペース（全角／半角）はを可視化
(setq whitespace-space-regexp "\\([\x0020|\u3000]+\\)")

(set-face-foreground 'whitespace-tab "#808000") ; "#4e9b4d")
(set-face-background 'whitespace-tab 'nil)
(set-face-underline  'whitespace-tab t)
(set-face-foreground 'whitespace-space "#808000") ; "#4e9b4d")
(set-face-background 'whitespace-space 'nil)
(set-face-bold-p 'whitespace-space t)
(set-face-foreground 'whitespace-newline  "#808000") ; "DimGray")
(set-face-background 'whitespace-newline 'nil)
(setq whitespace-action '(auto-cleanup)) ; 保存前に自動でクリーンアップ
(global-whitespace-mode 1)

;; バッファーリスト表示
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;; (global-set-key [f2] 'ibuffer)

;;----------------------------------------------------------
;; git-gutter+ git 変更分を表示
(require 'git-gutter+)
(global-git-gutter+-mode t)

;;----------------------------------------------------------
;; カレントのファイルパスをコピーする
;; - Emacs で開いているファイルのフルパスをミニバッファに表示とコピーするための emacs lisp の関数のメモ
;;   http://cortyuming.hateblo.jp/entry/20130802/p1
(defun get-file-path ()
  "show the full path file name in the minibuffer and copy to kill ring."
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))
    (message (buffer-file-name))))
(global-set-key (kbd "C-c .") 'copy-file-path)

;;----------------------------------------------------------
;; 外部エディタで編集されたファイルを検出して読み込み直し問い合わせる(C-c r)
(defun shk-deferred-auto-revert (&optional ignore-auto noconfirm)
    (interactive)
    (if (called-interactively-p)
        (progn
          (setq header-line-format nil)
          (let ((revert-buffer-function nil))
            (revert-buffer ignore-auto t)))
      (setq header-line-format
            (format "%s. Press C-c R to reload it"
                    (propertize "This file has changed on disk"
                                'face '(:foreground "#f00"))))))
(setq revert-buffer-function 'shk-deferred-auto-revert)
(global-set-key [(control c)(R)] 'shk-deferred-auto-revert)
;; (global-auto-revert-mode 1) ;; 勝手に読み直す。emasc側で変更があるとセーブできなくなる
(global-auto-revert-mode t)


;;----------------------------------------------------------
;; インクリメンタルサーチ
;; ;; 検索(全般)時には大文字小文字の区別する
;; (setq case-fold-search nil)
;; ;; インクリメンタルサーチ時には大文字小文字の区別する
;; (setq isearch-case-fold-search nil)
;; 検索時（先頭⇔末尾）ラップしない
(setq isearch-wrap-function (lambda () (error "no more matches")))
;; Tabで検索文字列を補完
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)

;;----------------------------------------------------------
;; 自動補完有効化
;; (require 'auto-complete)
;; (require 'auto-complete-config)
(ac-config-default)
(setq ac-use-menu-map t)
(ac-set-trigger-key "TAB")
(setq ac-delay 1.0) ; auto-completeまでの時間
(setq ac-auto-show-menu 1.0) ; メニューが表示されるまで
(global-auto-complete-mode t)
;; AC 有効にする追加のメジャーモード
(add-to-list 'ac-modes 'makefile-make-mode)
(add-to-list 'ac-modes 'makefile-gmake-mode)
(add-to-list 'ac-modes 'makefile-bsdmake-mode)

;;----------------------------------------------------------
;; 置換
;; (define-key global-map (kbd "C-x r") 'query-replace-regexp)
;; - visual-regexp-steroids.el : 【正規表現革命】isearchや置換でPerl/Pythonの正規表現を使おうぜ！
;;   http://emacs.rubikitch.com/visual-regexp-steroids/
(require 'visual-regexp)
(require 'visual-regexp-steroids)
;; (setq vr/engine 'python)                ;python regexpならばこれ
(setq vr/engine 'pcre2el)               ;elispでPCREから変換
(global-set-key (kbd "M-%") 'vr/query-replace)
;; (global-set-key (kbd "C-x r") 'vr/query-replace) ;; my keybind 削除予定
;; ;; multiple-cursorsを使っているならこれで
;; (global-set-key (kbd "C-c m") 'vr/mc-mark)
;; ;; 普段の正規表現isearch
(global-set-key (kbd "C-M-r") 'vr/isearch-backward)
(global-set-key (kbd "C-M-s") 'vr/isearch-forward)

;;----------------------------------------------------------
;; helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)

;; ;; 旧設定 2018/07/16 まで
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ;; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ;; make TAB work in terminal
;; (define-key helm-map (kbd "C-z")  'helm-select-action) ;; list actions using C-z
;; (define-key global-map (kbd "M-x")     'helm-M-x) ;; helm で M-x を実行
;; (define-key global-map (kbd "M-y") 'helm-show-kill-ring) ;; helm で M-y を実行(キリングバッファー一覧が表示され、選択してペーストできる）
;; (define-key global-map (kbd "C-c C-f") 'helm-find-files) ;; カーソル行がファイルパスの場合、そのファイルを開けて移動する
;; (define-key global-map (kbd "C-x b") 'helm-buffers-list) ;; buffer to switch をやめて helm-buffer-list をやめて helm-mini に変更 
;; (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char) ;; delete backword

;; 新設定 2018/07/17から
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key global-map (kbd "C-x b")   'helm-mini)
(define-key global-map (kbd "M-r")     'helm-resume)
(define-key global-map (kbd "C-M-h")   'helm-apropos)
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; Disable helm in some functions
;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
;; (add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-tag . nil))

;; (setq helm-buffer-details-flag nil)

;; Emulate `kill-line' in helm minibuffer
(setq helm-delete-minibuffer-contents-from-point t)
(defadvice helm-delete-minibuffer-contents (before emulate-kill-line activate)
  "Emulate `kill-line' in helm minibuffer"
  (kill-new (buffer-substring (point) (field-end))))

(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-file-exist activate)
  "Execute command only if CANDIDATE exists"
  (when (file-exists-p candidate)
    ad-do-it))

(setq helm-ff-fuzzy-matching nil)
(defadvice helm-ff--transform-pattern-for-completion (around my-transform activate)
  "Transform the pattern to reflect my intention"
  (let* ((pattern (ad-get-arg 0))
         (input-pattern (file-name-nondirectory pattern))
         (dirname (file-name-directory pattern)))
    (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
    (setq ad-return-value
          (concat dirname
                  (if (string-match "^\\^" input-pattern)
                      ;; '^' is a pattern for basename
                      ;; and not required because the directory name is prepended
                      (substring input-pattern 1)
                    (concat ".*" input-pattern))))))

(defun helm-buffers-list-pattern-transformer (pattern)
  (if (equal pattern "")
      pattern
    (let* ((first-char (substring pattern 0 1))
           (pattern (cond ((equal first-char "*")
                           (concat " " pattern))
                          ((equal first-char "=")
                           (concat "*" (substring pattern 1)))
                          (t
                           pattern))))
      ;; Escape some characters
      (setq pattern (replace-regexp-in-string "\\." "\\\\." pattern))
      (setq pattern (replace-regexp-in-string "\\*" "\\\\*" pattern))
      pattern)))

(unless helm-source-buffers-list
  (setq helm-source-buffers-list
        (helm-make-source "Buffers" 'helm-source-buffers)))
(add-to-list 'helm-source-buffers-list
             '(pattern-transformer helm-buffers-list-pattern-transformer))

(defadvice helm-ff-sort-candidates (around no-sort activate)
  "Don't sort candidates in a confusing order!"
  (setq ad-return-value (ad-get-arg 0)))

;; --- helm setting end ---

(setq helm-buffer-max-length 50) ; buffer 名を広くする

;; - Windows NTEmacs で ag (The Silver Searcher) を使う
;;   http://extra-vision.blogspot.com/2016/01/ntemacs-ag-silver-searcher.html

;; (1) windows の場合、 helm-ag.el の以下の箇所を変更する必要がある
;;  helm-ag.elc があるので削除しておく
;;(defun helm-ag--init ()
;;  ;; (let ((buf-coding buffer-file-coding-system))
;;  (let ((buf-coding 'japanese-cp932-dos)) <--- これに変更
;;  (message "oya debug: helm-ag--init buf-coding: %s" buf-coding)

;; helm は pt を利用するので以下からダウンロードしpathの通ったところに保存しておくこと
;; - the_platinum_searcher
;;   https://github.com/monochromegane/the_platinum_searcher/releases
(setq helm-ag-base-command "pt --smart-case --nocolor --nogroup ")
;; (setq helm-ag-command-option "--output-encode sjis ")
;; (setq helm-ag-command-option "--all-text")
;; 検索対象外ファイルの指定( vc起動に生成される *.ncb, *.suo を除外しておかないとvc起動中は検索が失敗する。vcがファイルロックするから。。。slnファイルはロックされない)
;; helm-ag用 --ignoreオプション
(setq helm-ag-ignore-patterns '("*~" "#.*#" "GPATH" "GRTAGS" "GTAGS" "*.ncb" "*.suo"))
;; (setq helm-ag-always-set-extra-option t) ;; helm-do-ag 実行時のオプション入力許可
;; helm-do-ag用 --ignoreオプション
(setq helm-ag--extra-options "--ignore \"GPATH\" --ignore \"GRTAGS\" --ignore \"GTAGS\" --ignore \"*.ncb\" --ignore \"*.suo\" --ignore \"*.intermediate.manifest\" ")
;; helm-follow-mode （C-c C-f で ON/OFF）の前回の状態を維持する
;; ↑らしいけど、実際はチラ見がかってにプレビューされる状態になる
(setq helm-follow-mode-persistent t)
;; ;; 正規表現を有効にする
;; (setq helm-ag-use-emacs-lisp-regexp t)
;; helm ag 編集画面は一覧に表示させる
(setq helm-white-buffer-regexp-list '("helm-ag-edit"))

;; (1) を実施せずに、入力 utf-8 書き出しが cp932 に強制する
;; windowsのみ
(when (eq system-type 'windows-nt)
  (defun set-rw-coding-system:around (orig-func &rest args)
    (let ((coding-system-for-read  'utf-8-dos) ; 行末の ^M を避けるため -dos が必要
          (coding-system-for-write 'cp932-dos))
      (apply orig-func args)          ; オリジナル関数を呼び出し
      ))
  (advice-add 'process-file :around #'set-rw-coding-system:around)
  )
;; (setq default-process-coding-system '(utf-8-unix . cp932-dos))

;; ;; helm で find-files を実行
;;  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
;; ;; Helm はタブに helm-select-action が割り当てられているので、基本的にファイル名を補完する目的でタブを押しても意味がありません。
;; ;; タブでファイル名を補完したい場合はタブに helm-execute-persistent-action（C-z を押した時に実行されるコマンド）を割り当てておきます。
;; ; For find-file etc.
;; (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; ;; For helm-find-files etc.
;; (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

;; grep は helm-ag で代用
;; (define-key global-map (kbd "C-x g") 'helm-ag)

;;----------------------------------------------------------
;; helm-swoop は migemo もあわせて利用するのがいいらしいが、日本語入力を手間だと思っていないので migemo は捨てる。
;; 参考 : http://rubikitch.com/2014/12/25/helm-swoop/
;;      : http://fukuyama.co/helm-swoop
;; helm-swoop を require しないと駄目っぽい。
(require 'helm-swoop)
;; キーバインドはお好みで
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;; isearch実行中にhelm-swoopに移行
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; helm-swoop実行中にhelm-multi-swoop-allに移行
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)
;; 値がtの場合はウィンドウ内に分割、nilなら別のウィンドウを使用
(setq helm-swoop-split-with-multiple-windows nil)
;; ウィンドウ分割方向 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)
;; (setq helm-swoop-split-direction 'split-window-horizontally)
;; nilなら一覧のテキストカラーを失う代わりに、起動スピードをほんの少し上げる
(setq helm-swoop-speed-or-color t)

;;----------------------------------------------------------
;; gtags-mode をやめて helm-gtags に変更 2016/01/05
;; helm-gtags
;; (require 'helm-gtags)
;; path指定
;; 'root(タグファイルがあるディレクトリを頂点としたパス)
;; 'absolete(絶対パス)
;; 'relative(カレントディレクトリからの相対パス)
(setq gtags-path-style 'relative)
(setq helm-gtags-path-style 'relative)
;; key bindings
(add-hook 'helm-gtags-mode-hook
          '(lambda ()
              (local-set-key (kbd "M-t") 'helm-gtags-find-tag)
              (local-set-key (kbd "M-r") 'helm-gtags-find-rtag)
              (local-set-key (kbd "M-s") 'helm-gtags-find-symbol)
              (local-set-key (kbd "C-t") 'helm-gtags-pop-stack)))

(add-hook 'ruby-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'js-mode-hook 'helm-gtags-mode)
(add-hook 'csharp-mode-hook 'helm-gtags-mode)
;; タグを自動更新。windowsの場合、動作しないかも。。。
(setq helm-gtags-auto-update t)

;;----------------------------------------------------------

;; ツリー表示いろいろ;
(require 'popwin)
;; (setq display-buffer-function 'popwin:display-buffer)
;; (setq popwin:popup-window-position 'bottom)

(require 'direx)
(push '(direx:direx-mode :position left :width 50 :dedicated t) popwin:special-display-config)
(global-set-key (kbd "C-c t d") 'direx:jump-to-directory-other-window)
(global-set-key (kbd "C-c t p") 'direx-project:jump-to-project-root-other-window)
;; (global-set-key [f7] 'direx:find-directory)
(global-set-key (kbd "C-c t b") 'ibuffer)
;; (defun tree-jump ()
;;   "direx:jump-to-directory-other-window"
;;   (interactive)
;;   ( direx:jump-to-directory-other-window ))
;; (defun tree-project ()
;;   "direx-project:jump-to-project-root-other-window"
;;   (interactive)
;;   ( direx-project:jump-to-project-root-other-window ))
;; ;; (defun tree-find ()
;; ;;   "direx:find-directory"
;; ;;   (interactive)
;; ;;   ( direx:find-directory ))
;; (defun tree-buffer ()
;;   "ibuffer"
;;   (interactive)
;;   ( ibuffer ))

;; ------------------------------------------------------------------------------
;; Meadow時代のofficeファイルをテキスト化するパッケージ（独自改良版）
(require 'xdoc2txt)

;;----------------------------------------------------------
;; ruby-mode
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq ruby-indent-level tab-width)
             (setq ruby-deep-indent-paren-style nil)
             ;; (local-set-key (kbd "RET") 'newline-and-indent)
             ;; (local-set-key "\M-t" 'find-tag) ;関数の定義元へ
             ;; (local-set-key "\C-t" 'pop-tag-mark) ;前のバッファに戻る
             ;; (flycheck-mode t)
             ;; (helm-gtags-mode)
             )
          )
;;----------------------------------------------------------
;; python-mode
(add-hook 'python-mode-hook
    '(lambda ()
        (setq python-indent 2)
        (setq indent-tabs-mod nil)
        ;; (define-key (current-local-map) "\C-h" 'python-backspace)
    ))
;;----------------------------------------------------------
;; js-mode
(add-hook 'js-mode-hook
          '(lambda()
             (setq tab-width 2)
             (setq js-indent-level 2)
             (setq indent-tabs-mode nil)
             ;; (local-set-key (kbd "RET") 'newline-and-indent)
             ;; (gtags-mode 1)
             ;;; 2016/12/02 以下を有効にすると特定の日本語コメントでemacsが固まる。
             ;;; (flycheck-mode t)
             ;;; (flycheck-add-next-checker 'javascript-jshint
             ;;;                            'javascript-gjslint)
             )
          )

;;------------------------------------------------------------------------------
;; C/C++ mode
;; ;; Qt 考慮する
;; (setq c-C++-access-key "\\<\\(slots\\|signals\\|private\\|protected\\|public\\)\\>[ \t]*[(slots\\|signals)]*[ \t]*:")
;; (font-lock-add-keywords 'c-mode '(("\\<\\(Q_OBJECT\\|public slots\\|public signals\\|private slots\\|private signals\\|protected slots\\|protected signals\\)\\>" . font-lock-constant-face)))
(add-hook 'c-mode-hook
          '(lambda()
             (setq c-set-style "stroustrup")
             (c-set-offset 'innamespace 0)
             (c-set-offset 'brace-list-open 0)
             (c-set-offset 'brace-list-intro 4)
             (setq tab-width 4)
             (setq indent-tabs-mode t)
             (setq c-basic-offset 4)
             (setq c-tab-always-indent t)
             (local-set-key (kbd "RET") 'newline-and-indent)
             )
          )

(add-hook 'c++-mode-hook
          '(lambda()
             (setq c-set-style "stroustrup")
             (c-set-offset 'innamespace 0)
             (c-set-offset 'brace-list-open 0)
             (c-set-offset 'brace-list-intro 4)
             (setq tab-width 4)
             (setq indent-tabs-mode t)
             (setq c-basic-offset 4)
             (setq c-tab-always-indent t)
             (local-set-key (kbd "RET") 'newline-and-indent)
             )
          )

;;------------------------------------------------------------------------------
;; csharp mode
(add-hook 'csharp-mode-hook
          '(lambda()
             ;; (setq c-set-style "cc-mode")
             ;; (c-set-offset 'innamespace 0)
             (setq tab-width 4)
             (setq indent-tabs-mode nil)
             ;; (setq c-basic-offset 4)
             ;; (setq c-tab-always-indent t)
             ;; (local-set-key (kbd "RET") 'newline-and-indent)
             (auto-complete-mode)
             )
          )
;; (setq csharp-want-imenu nil)

;;----------------------------------------------------------
;; markdown-mode
(add-to-list 'auto-mode-alist'("\\.md\\'" . markdown-mode))

;;----------------------------------------------------------
;; vb.net-mode
(autoload 'vbnet-mode "vbnet-mode" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
                                 vbnet-mode)) auto-mode-alist))

(defun my-vbnet-mode-fn ()
  "My hook for VB.NET mode"
  (setq indent-tabs-mode t
        tab-width 4)
  (remove-hook 'local-write-file-hooks 'vbnet-untabify t)
  )
(add-hook 'vbnet-mode-hook 'my-vbnet-mode-fn)
;; vbnet-mode.el の (add-hook 'local-write-file-hooks 'vbnet-untabify) を無効にするため用
;; これ無効にしないとファイル保存時にハードタブをソフトタブに見たままに変更し保存してしまう。

;;----------------------------------------------------------
;; web-mode
(add-hook 'web-mode-hook
          '(lambda ()
             (setq web-mode-attr-indent-offset nil)
             (setq web-mode-markup-indent-offset 2)
             (setq web-mode-css-indent-offset 2)
             (setq web-mode-code-indent-offset 2)
             (setq web-mode-sql-indent-offset 2)
             (setq indent-tabs-mode nil)
             (setq tab-width 2)
             ))
(add-to-list 'auto-mode-alist'("\\.html\\'" . web-mode))

;;----------------------------------------------------------
;; makefile-gmake-mode
;; (setq-default tab-always-indent t)
(defun hard-tabs ()
  ;; (setq-default indent-tabs-mode t)
  (setq indent-tabs-mode t
        tab-width 2))
(add-hook 'makefile-mode-hook 'hard-tabs)
(add-hook 'makefile-gmake-mode-hook 'hard-tabs)
(add-hook 'makefile-bsdmake-mode-hook 'hard-tabs)
(setq auto-mode-alist (append '(("\\(make\\.\\|makefile\\).*$" .
                                 makefile-mode)) auto-mode-alist))

;; 大文字／小文字変換。なんか以下が必要っぽい
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;----------------------------------------------------------
;; GDB 関連
;; 有用なバッファを開くモード
(setq gdb-many-windows t)
;; ;; 変数の上にマウスカーソルを置くと値を表示
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
;; I/O バッファを表示
(setq gdb-use-separate-io-buffer t)
;; t にすると mini buffer に値が表示される
(setq gud-tooltip-echo-area nil)

;; Visual Studio Debugger Key Bindings
(global-set-key (kbd "<f10>") 'gud-next)
(global-set-key (kbd "<f11>") 'gud-step)
(global-set-key [(f9)] 'gud-break)
(global-set-key [(shift f9)] 'gud-remove)
(global-set-key [(control f5)] 'gud-run)
(global-set-key [(f5)] 'gud-cont)
(global-set-key [(shift f5)] 'gud)
