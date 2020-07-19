(message "OS is %s." system-type)
(message "HOME is %s." (getenv "HOME"))
(message "PATH is %s." (getenv "PATH"))
(setenv "LC_MESSAGES" "C")

;; (setq url-proxy-services
;;       '(("http" . "172.17.10.213:8080")
;;         ("https" . "172.17.10.213:8080")))

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/elisp")

;;----------------------------------------------------------
;; 文字コード指定
(require 'cl-lib) ;; emacs 標準
(setenv "LANG" "ja_JP.UTF-8")

;;----------------------------------------------------------
;; windows 
(when (eq system-type 'windows-nt)
  ;;----------------------------------------------------------
  ;; パス指定
  (setenv "PATH"
          (concat
           "C:\\msys64\\mingw64\\bin" ";"
           (concat (getenv "HOME") "\\.emacs.d\\bin;")
           (getenv "PATH")))
  (setq exec-path (parse-colon-path (getenv "PATH"))) ;; 実行パスも同じにする
  
  ;; IME の設定をした後には実行しないこと
  ;; (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8-unix)
  (set-file-name-coding-system 'cp932)
  (setq locale-coding-system 'utf-8-unix)
  ;; プロセスが出力する文字コードを判定して、process-coding-system の DECODING の設定値を決定する
  (setq default-process-coding-system '(undecided-dos . utf-8-unix))
  ;; サブプロセスに渡すパラメータの文字コードを cp932 にする
  (cl-loop for (func args-pos) in '((call-process        4)
                                    (call-process-region 6)
                                    (start-process       3))
           do (eval `(advice-add ',func
                                 :around (lambda (orig-fun &rest args)
                                           (setf (nthcdr ,args-pos args)
                                                 (mapcar (lambda (arg)
                                                           (if (multibyte-string-p arg)
                                                               (encode-coding-string arg 'cp932)
                                                             arg))
                                                         (nthcdr ,args-pos args)))
                                           (apply orig-fun args))
                                 '((depth . 99)))))
  
  )

;;----------------------------------------------------------
;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)

;;----------------------------------------------------------
;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;;----------------------------------------------------------
;; ドラキュラテーマ
(use-package dracula-theme
  :ensure t
  :load-path "themes"
  :config
  (load-theme 'dracula t)
  )

;;----------------------------------------------------------
;; 複数ウィンドウを開かないようにする
;; (setq ns-pop-up-frames nil)

;;----------------------------------------------------------
;; 起動メッセージを表示しない
(setq inhibit-startup-message t)

;;----------------------------------------------------------
;; *scratch* buffer のメッセージを消す。
;; (setq initial-scratch-message t)

;;----------------------------------------------------------
;; ツールバーを非表示
(tool-bar-mode 0)

;;----------------------------------------------------------
;; キャレットが '(' や ')' の直後にある場合に対応する括弧を強調
(show-paren-mode t)
(setq show-paren-delay 0)

;;----------------------------------------------------------
;; ウィンドウ幅で折り返さない設定
;; 通常のウィンドウ用の設定
(setq-default truncate-lines t)
;;----------------------------------------------------------
;; ウィンドウを左右に分割したとき用の設定
(setq-default truncate-partial-width-windows t)

;;----------------------------------------------------------
;; ;; 勝手にウィンドウを分割するのをやめる
;; (setq split-width-threshold nil)
;; (setq split-height-threshold nil)

;;----------------------------------------------------------
;; タイトルバーに編集中のファイルのパス名を表示
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;;----------------------------------------------------------
;; ウィンドウ移動
;; shift ↑↓←→
(setq windmove-wrap-around t)
(windmove-default-keybindings)

;;----------------------------------------------------------
;; シフト＋矢印で範囲選択
;; (require 'pc-select)
;; (setq pc-select-selection-keys-only t)
;; (pc-selection-mode 1)

;;----------------------------------------------------------
;; 矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil) ;; 変なキーバインド禁止



;;----------------------------------------------------------
;; 移動が遅くなる問題を回避
;; 以下の手順で line-move が遅いことが判断できる
;;  1. M-x: profiler-start
;;  2.  ~ 何かしら操作 ~
;;  3. M-x: profiler-report
;; - Emacs point(cursor) movement lag
;;   https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag
;; - 非常に遅いEmacsをトラブルシューティングするにはどうすればよいですか？
;;   https://qastack.jp/emacs/5359/how-can-i-troubleshoot-a-very-slow-emacs
;; - 非常に長い行がEmacsを遅くするのを防ぐにはどうすればよいですか？
;;   https://qastack.jp/emacs/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
(setq auto-window-vscroll nil)

;;----------------------------------------------------------
;; powerline フォント
(when (eq system-type 'windows-nt)
  (set-face-font 'default "Ricty Diminished for Powerline-12")
  )


;;----------------------------------------------------------
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

;;----------------------------------------------------------
;; ホイールでスクロールする行数を設定
;; - マウスホイールでのスクロールを超快適にする3つの変数設定
;;   http://emacs.rubikitch.com/mouse-wheel/
(setq  mouse-wheel-scroll-amount '(2 ((shift) . 5) ((control)))
       ;; 速度を無視する
       mouse-wheel-progressive-speed nil)
;; スクロール時にカーソル位置を一定にする
(setq scroll-preserve-screen-position 'always)

;;----------------------------------------------------------
;; neotree 設定
;; 
;; - neotree-toggle toggle/hide NeoTree window
;; - neotree-stretch-toggle Maximize / Minimize
;; - n next line ， p previous line
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
(use-package all-the-icons
  :ensure t
  )
(use-package neotree
  :ensure t
  )
;; C-x }, C-x { でwindowサイズを変更できるよにする
(setq neo-window-fixed-size nil)
;; neotreeでファイルを新規作成した場合のそのファイルを開く
(setq neo-create-file-auto-open t)
;; delete-other-window で neotree ウィンドウを消さない
(setq neo-persist-show t)

;;----------------------------------------------------------
;; タブの挙動(一般的なタブの挙動） ※これを無効にすると、tab-mark が表示されなくなる
(setq indent-line-function 'tab-to-tab-stop)

;;----------------------------------------------------------
;; バックデリート有効
(global-set-key "\C-h" 'delete-backward-char)


;;----------------------------------------------------------
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

;;----------------------------------------------------------
;; tabbarはwindowシステムの場合のみ
(if window-system (progn
                    ;; tabbar有効化
                    (use-package tabbar
                      :ensure t
                      ;; :bind (("M-<right>" . tabbar-forward-tab)
                      ;;      ("M-<left>" . tabbar-backward-tab))
                      :config
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
                      )
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
;; (use-package hl-line)
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

;;----------------------------------------------------------
;; 対応する括弧をハイライト
(show-paren-mode t)

;;----------------------------------------------------------
;; 空白関係
(use-package whitespace
  :ensure t
  )

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

;; ;;----------------------------------------------------------
;; ;; スペース（全角／半角）はを可視化
;; (setq whitespace-space-regexp "\\([\x0020|\u3000]+\\)")

;; (set-face-foreground 'whitespace-tab "#808000") ; "#4e9b4d")
;; (set-face-background 'whitespace-tab 'nil)
;; (set-face-underline  'whitespace-tab t)
;; (set-face-foreground 'whitespace-space "#808000") ; "#4e9b4d")
;; (set-face-background 'whitespace-space 'nil)
;; (set-face-bold-p 'whitespace-space t)
;; (set-face-foreground 'whitespace-newline  "#808000") ; "DimGray")
;; (set-face-background 'whitespace-newline 'nil)
;; (setq whitespace-action '(auto-cleanup)) ; 保存前に自動でクリーンアップ
(global-whitespace-mode 1)


;;----------------------------------------------------------
;; 改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;;----------------------------------------------------------
;; 文字エンコーディングの文字列表現
;; - モードラインの文字エンコーディング表示をわかりやすくする
;;   https://qiita.com/kai2nenobu/items/ddf94c0e5a36919bc6db
(defun my-coding-system-name-mnemonic (coding-system)
  (let* ((base (coding-system-base coding-system))
         (name (symbol-name base)))
    (cond ((string-prefix-p "utf-8" name) "U8")
          ((string-prefix-p "utf-16" name) "U16")
          ((string-prefix-p "utf-7" name) "U7")
          ((string-prefix-p "japanese-shift-jis" name) "SJIS")
          ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
          ((string-match "japanese-iso-8bit" name) "EUC")
          (t "???")
          )))

(defun my-coding-system-bom-mnemonic (coding-system)
  (let ((name (symbol-name coding-system)))
    (cond ((string-match "be-with-signature" name) "[BE]")
          ((string-match "le-with-signature" name) "[LE]")
          ((string-match "-with-signature" name) "[BOM]")
          (t ""))))

(defun my-buffer-coding-system-mnemonic ()
  "Return a mnemonic for `buffer-file-coding-system'."
  (let* ((code buffer-file-coding-system)
         (name (my-coding-system-name-mnemonic code))
         ;; (name code)
         (bom (my-coding-system-bom-mnemonic code)))
    (format "%s%s" name bom)))

;; `mode-line-mule-info' の文字エンコーディングの文字列表現を差し替える
(setq-default mode-line-mule-info
              (cl-substitute '(:eval (my-buffer-coding-system-mnemonic))
                             "%z" mode-line-mule-info :test 'equal))

;;----------------------------------------------------------
;; 行番号
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;;----------------------------------------------------------
;; ;; beep音を消す
;; (defun my-bell-function ()
;;   (unless (memq this-command
;;         '(isearch-abort abort-recursive-edit exit-minibuffer
;;               keyboard-quit mwheel-scroll down up next-line previous-line
;;               backward-char forward-char))
;;     (ding)))
;; (setq ring-bell-function 'my-bell-function)

;; 警告音もフラッシュも全て無効(警告音が完全に鳴らなくなるので注意)
(setq ring-bell-function 'ignore)

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
;; 置換
;; (define-key global-map (kbd "C-x r") 'query-replace-regexp)
;; - visual-regexp-steroids.el : 【正規表現革命】isearchや置換でPerl/Pythonの正規表現を使おうぜ！
;;   http://emacs.rubikitch.com/visual-regexp-steroids/
(use-package visual-regexp
  :ensure t
  )
(use-package visual-regexp-steroids
  :ensure t
  )
(use-package pcre2el
  :ensure t
  )
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
(use-package wgrep
  :ensure t
  )
(setq wgrep-auto-save-buffer t)

(use-package fzf
  :ensure t
  )

(use-package counsel
  :ensure t
  )

(use-package ivy
  :ensure t
  )

(when (require 'ivy nil t)
  ;; M-o を ivy-hydra-read-action に割り当てる．
  (when (require 'ivy-hydra nil t)
    (setq ivy-read-action-function #'ivy-hydra-read-action))
  
  ;; `ivy-switch-buffer' (C-x b) のリストに recent files と bookmark を含める．
  ;; https://tam5917.hatenablog.com/entry/2019/12/31/094621
  ;; (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
  (setq ivy-use-virtual-buffers t)
  
  ;; ミニバッファでコマンド発行を認める
  (when (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode 1)) ;; 何回層入ったかプロンプトに表示．

  ;; ESC連打でミニバッファを閉じる
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)

  ;; プロンプトの表示が長い時に折り返す（選択候補も折り返される）
  (setq ivy-truncate-lines nil)

  ;; リスト先頭で `C-p' するとき，リストの最後に移動する
  (setq ivy-wrap t)

  ;; アクティベート
  (ivy-mode 1))

(when (require 'counsel nil t)

  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)
  (global-set-key (kbd "C-M-z") 'counsel-fzf)
  (global-set-key (kbd "C-M-r") 'counsel-recentf)
  (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
  (global-set-key (kbd "C-M-f") 'counsel-ag)

  ;; アクティベート
  (counsel-mode 1))


;;----------------------------------------------------------
(use-package swiper
  :ensure t
  )
(when (require 'swiper nil t)
  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point))


;;----------------------------------------------------------
;; シンボルハイライト
(use-package symbol-overlay
  :ensure t
  )
(when (require 'symbol-overlay nil t)
  (add-hook 'prog-mode-hook #'symbol-overlay-mode)
  (add-hook 'markdown-mode-hook #'symbol-overlay-mode)
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-map (kbd "p") 'symbol-overlay-jump-prev) ;; 次のシンボルへ
  (define-key symbol-overlay-map (kbd "n") 'symbol-overlay-jump-next) ;; 前のシンボルへ
  (define-key symbol-overlay-map (kbd "C-g") 'symbol-overlay-remove-all) ;; ハイライトキャンセル
  )

;;----------------------------------------------------------
;; dumb-jump
(use-package dumb-jump
  :ensure t
  )
(when (require 'dumb-jump nil t)
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)
  )

;;----------------------------------------------------------
;; git-gutter+ git 変更分を表示
(use-package git-gutter+
  :ensure t
  )
(global-git-gutter+-mode t)

;;----------------------------------------------------------
(use-package magit
  :ensure t)

(with-eval-after-load "magit"
  (setq magit-completing-read-function 'ivy-completing-read))

;;----------------------------------------------------------
;; flycheck
(use-package flycheck
  :config
  (when (locate-library "flycheck-irony")
    (flycheck-irony-setup))
  (global-flycheck-mode t)
  )

(use-package cc-mode
  :ensure t
  )
(use-package yasnippet
  :ensure t
  )

;;(yas-global-mode)

;;(use-package irony
;;  :ensure t
;;  )

(use-package irony
  :ensure t
  :commands irony-mode
  :init
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'c++-mode-hook 'irony-mode)
  :config
  ;; C言語用にコンパイルオプションを設定する.
  (add-hook 'c-mode-hook
            '(lambda ()
               (setq irony-additional-clang-options '("-std=c11" "-Wall" "-Wextra"))))
  ;; C++言語用にコンパイルオプションを設定する.
  (add-hook 'c++-mode-hook
            '(lambda ()
               (setq irony-additional-clang-options '("-std=c++14" "-Wall" "-Wextra"))))
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  ;; Windows環境でパフォーマンスを落とす要因を回避.
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  ;; バッファサイズ設定(default:4KB -> 64KB)
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
  )

(use-package company
  :ensure t
  )

(use-package company-irony-c-headers
  :ensure t
  )

(use-package company-irony
  :ensure t
  :config
  ;; companyの補完のバックエンドにironyを使用する.
  (add-to-list 'company-backends '(company-irony-c-headers company-irony))
  )

(global-company-mode) ; 全バッファで有効にする

(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)

(use-package csharp-mode
  :ensure t
  )
;;(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;;(setq auto-mode-alist
;;      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;;----------------------------------------------------------
;; windows 
(when (eq system-type 'windows-nt)
  ;; omnisharp を利用する場合、初回だけ以下を実施する必要がある
  ;;  M-x: omnisharp-install-server
  ;; 実施すると、
  ;; .emacs.d\.cache\omnisharp\server
  ;; にomnisharpに必要な .net 環境がインストールされる。
  ;; 2020/5/1 時点では v1.34.5 ディレクトリが作成される。
  ;; そこに .net 環境がインストールされる。(\.emacs.d\.cache\omnisharp\server\v1.34.5\...)
  (use-package omnisharp
    :ensure t
    )
  
  ;;--- c# ---
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp))
  
  (defun my-csharp-mode-setup ()
    (setq auto-mode-alist
          (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
    (omnisharp-mode)
    (company-mode)
    (flycheck-mode)
    (turn-on-eldoc-mode)
    
    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq evil-shift-width 4)
    
    ;; csharp-mode README.md recommends this too
    ;; (electric-pair-mode 1)       ;; Emacs 24
    ;; (electric-pair-local-mode 1) ;; Emacs 25
    
    (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
    (local-set-key (kbd "C-c C-c") 'recompile)
    )
  
  (setq omn​​isharp-company-strip-trailing-brackets nil)
  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
  )

(defun my-emacs-lisp-mode-setup ()
  (setq indent-tabs-mode nil)
  )

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-setup t)

;; ------------------------------------------------------------------------------
;; markdown
(use-package markdown-mode
  :ensure t
  )
(when (require 'markdown-mode nil t)
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  ;; コードブロックのハイライト化
  (setq markdown-fontify-code-blocks-natively t)
  )

;; ------------------------------------------------------------------------------
;; Meadow時代のofficeファイルをテキスト化するパッケージ（独自改良版）
(use-package xdoc2txt
  :load-path "~/.emacs.d/elisp"
  )


;;--- end of my settings ---
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (omnisharp csharp-mode company-irony company-irony-c-headers company irony yasnippet magit git-gutter+ dumb-jump symbol-overlay counsel fzf wgrep pcre2el visual-regexp-steroids visual-regexp tabbar neotree all-the-icons dracula-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
