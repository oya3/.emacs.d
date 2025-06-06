(message "OS is %s." system-type)
(message "HOME is %s." (getenv "HOME"))
(message "PATH is %s." (getenv "PATH"))
(setenv "LC_MESSAGES" "C")

(setq package-check-signature nil)

;; ログ表示
;; https://www.grugrut.net/posts/my-emacs-init-el/
(setq debug-on-error t)

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

;; カーソル位置がずれるので無効。2023/9/4
;; ;;----------------------------------------------------------
;; ;; 文字コード指定
;; (require 'cl-lib) ;; emacs 標準
;; (set-language-environment "Japanese")
;; (prefer-coding-system 'utf-8)
;; ;; (setenv "LANG" "ja_JP.UTF-8")
;; (set-coding-system-priority 'utf-8 'euc-jp 'iso-2022-jp 'cp932)

;;----------------------------------------------------------
;; windows
(when (eq system-type 'windows-nt)
  ;; ;;----------------------------------------------------------
  ;; ;; パス指定
  ;; (setenv "PATH"
  ;;         (concat
  ;;          "C:\\msys64\\mingw64\\bin" ";"
  ;;          (concat (getenv "HOME") "\\.emacs.d\\bin;")
  ;;          ;; (concat (getenv "HOME") "\\.rbenv\\versions\\2.5.8\\bin;")
  ;;          (getenv "PATH")))
  ;; (setq exec-path (parse-colon-path (getenv "PATH"))) ;; 実行パスも同じにする

  ;; ;;shell for msys64/mingw64
  ;; (setq shell-file-name "C:\\msys64\\usr\\bin\\bash.exe")
  ;; (setenv "SHELL" shell-file-name)
  ;; (setq explicit-shell-file-name shell-file-name)
  ;; ;; (setq explicit-shell-file-name "C:\\msys64\\usr\\bin\\bash.exe")
  ;; ;; (setq explicit-bash.exe-args '("--login" "-i"))
  ;; windows 側から起動した場合の対策
  (let ((paths '("C:/Program Files/Git/mingw64/bin"
                 "C:/Program Files/Git/usr/local/bin"
                 "C:/Program Files/Git/usr/bin"
                 "C:/Program Files/Git/usr/bin"
                 "C:/Program Files/Git/mingw64/bin"
                 "C:/Program Files/Git/usr/bin")))
    (dolist (path paths)
      (when (and (file-directory-p path)
                 (not (member path exec-path)))
        (add-to-list 'exec-path path))))
  
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

;;----------------------------
;; terminal 時のマウス設定
;; https://nodamotoki.hatenablog.com/entry/2016/11/12/155311
;; https://www.yokoweb.net/2016/12/25/emacs-mac-win-select/
(if (not window-system) (progn
                          (xterm-mouse-mode t)
                          (mouse-wheel-mode t)
                          (menu-bar-mode -1)
                          (global-set-key [mouse-4] '(lambda () (interactive) (scroll-down 3)))
                          (global-set-key [mouse-5] '(lambda () (interactive) (scroll-up   3)))
                          ))

;; コピーバッファ共有 for mac
;; 以下の実施が必要
;; $ brew install reattach-to-user-namespace
(when (eq system-type 'darwin)
    (progn
      (defun copy-from-osx ()
    (shell-command-to-string "reattach-to-user-namespace pbpaste"))
      (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "reattach-to-user-namespace" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
      (setq interprogram-cut-function 'paste-to-osx)
      (setq interprogram-paste-function 'copy-from-osx)
    )
    (message "This platform is not mac")
)

(when (eq system-type 'darwin)
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize))
  )

;; ;; CommandとOptionを入れ替える for mac
;; (when (equal system-type 'darwin)
;;   (when (memq window-system '(mac ns))
;;     (setq mac-option-modifier 'meta)
;;     (setq mac-command-modifier 'super)
;;     )

;; 選択時の背景色
(set-face-attribute 'region nil :background "#999999")

;; ;;----------------------------
;; ;; クリップボードに反映する
;; ;; https://blog.misosi.ru/2017/01/17/osc52e-el/
;; (if (not window-system) (progn
;;                           (require 'osc52e)
;;                           (osc52-set-cut-function)
;;                           ))
;; osc52e.el から拡張されたやつ
;; https://blog.misosi.ru/2017/01/17/osc52e-el/
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode)
  :bind ("M-w" . clipetty-kill-ring-save)
  )

;; ;; バッファを移動したときに、カーソル位置を一瞬だけ強調してわかりやすくする。
;; (use-package beacon
;;   :ensure t
;;   :diminish beacon-mode
;;   :config
;;   (beacon-mode 1))

;; ;; ヤンクした場合などに編集箇所を強調表示してわかりやすくする。
;; ;; volatile-highlightsの設定
;; (use-package volatile-highlights
;;   :ensure t
;;   :diminish volatile-highlights-mode
;;   :config
;;   (volatile-highlights-mode t))

;; ;; インデント表示
;; ;; highlight-indent-guidesの設定
;; (use-package highlight-indent-guides
;;   :ensure t
;;   :diminish highlight-indent-guides-mode
;;   :custom
;;   (highlight-indent-guides-method 'character)
;;   (highlight-indent-guides-auto-character-face-perc 20)
;;   (highlight-indent-guides-character ?\|)
;;   :hook
;;   (prog-mode . highlight-indent-guides-mode))

;; ;; ファイル最終行以降をわかりやすく
;; ;; vim風に、最終行以降に~を表示する。 これはfringeに表示するので、行番号表示とずれてしまうのが難点。
;; ;; vi風に空行に~を表示する
;; (use-package vi-tilde-fringe
;;   :ensure t
;;   :diminish vi-tilde-fringe-mode
;;   :commands vi-tilde-fringe-mode
;;   :config
;;   (global-vi-tilde-fringe-mode))

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
(define-key global-map (kbd "C-x @") 'cua-set-rectangle-mark)


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

;; ;;----------------------------------------------------------
;; ;; powerline フォント
;; (when (eq system-type 'windows-nt)
;;   (set-face-font 'default "Ricty Diminished for Powerline-12")
;;   )

;; ;;----------------------------------------------------------
;; ;; grep, find を windows でも使えるようにmsys側を指定する for windows
;; (when (eq system-type 'windows-nt)
;;   (setq find-program "\"C:\\msys64\\usr\\bin\\find.exe\""
;;         grep-program "\"C:\\msys64\\usr\\bin\\grep.exe\""
;;         null-device "/dev/null")
;;   )

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

;; エスケープ文字表示
(setq ctl-arrow nil)

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

;; neotree を表示してwindows 1 番目に移動する
(use-package neotree
  :ensure t
  :config
  (add-hook 'emacs-startup-hook
            (lambda ()
              (neotree-toggle)
              (other-window 1)))
  )


;; C-x }, C-x { でwindowサイズを変更できるよにする
;; windows は幅が広すぎるので固定を有効にしておく
(unless (eq system-type 'windows-nt)
  (setq neo-window-fixed-size nil)
  )
;; neotreeでファイルを新規作成した場合のそのファイルを開く
(setq neo-create-file-auto-open t)
;; delete-other-window で neotree ウィンドウを消さない
(setq neo-persist-show t)
;; 隠しファイルを無効(neotree-hidden-file-toggle)
(setq neo-show-hidden-files t)

;;----------------------------------------------------------
;; タブの挙動(一般的なタブの挙動） ※これを無効にすると、tab-mark が表示されなくなる
(setq indent-line-function 'tab-to-tab-stop)

(when (boundp 'indent-rigidly-map)
  ;; 矢印ではなく f b で簡単にインデントできるようにする
  (define-key indent-rigidly-map (kbd "f") 'indent-rigidly-right-to-tab-stop)
  (define-key indent-rigidly-map (kbd "b") 'indent-rigidly-left-to-tab-stop)

  ;; 1文字ずつインデントする方は滅多に使わないので shift 付きにする
  (define-key indent-rigidly-map (kbd "F") 'indent-rigidly-right)
  (define-key indent-rigidly-map (kbd "B") 'indent-rigidly-left)

  ;; C-x C-i を2連続でタイプした場合は使用頻度が高い右タブインデントに割り当て
  (define-key indent-rigidly-map (kbd "C-x TAB") 'indent-rigidly-right-to-tab-stop)

  ;; vi ライクなキーバインドにしたいときは h l を使うのもいいかも
  ;; (define-key indent-rigidly-map (kbd "l") 'indent-rigidly-right-to-tab-stop)
  ;; (define-key indent-rigidly-map (kbd "h") 'indent-rigidly-left-to-tab-stop)
  )

;;----------------------------------------------------------
;; バックデリート有効
(global-set-key "\C-h" 'delete-backward-char)

;;----------------------------------------------------------
;; 改行時の勝手インデント禁止
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

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
;; ;; コメント追加を無効
;; (global-unset-key "\M-;")
;; ;;(global-set-key (kbd "C-c ;") 'indent-for-comment)

;; ;; リージョン コメント＆アンコメント
;; (global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
;; ;; １行コメント＆アンコメント
;; (defun one-line-comment ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line)
;;     (set-mark (point))
;;     (end-of-line)
;;     (comment-or-uncomment-region (region-beginning) (region-end))))
;; (global-set-key (kbd "M-;") 'one-line-comment)

;; リージョン コメント＆アンコメント
(global-unset-key "\M-;")
(defun region-or-line-comment-or-uncomment ()
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (save-excursion
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)
      (comment-or-uncomment-region (region-beginning) (region-end)))))
(global-set-key (kbd "M-;") 'region-or-line-comment-or-uncomment)


;;----------------------------------------------------------
;; タブは半角スペースにする
;;----------------------------------------------------------
(setq-default tab-width 4 indent-tabs-mode nil)

;;----------------------------------------------------------
;; カラム数表示
;; - カラムがゼロ始まりなのが奇妙。rubocopと考え違うらしい： https://github.com/bbatsov/rubocop/issues/276
(column-number-mode t)

;;----------------------------------------------------------
;; カーソル位置(現在行)をハイライト
(global-hl-line-mode t)
(set-face-background hl-line-face "#444")

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
;                         spaces         ; スペース
;                         space-mark     ; 表示のマッピング
;                         empty
                         newline        ; 改行
                         newline-mark
                         ))

;; (setq whitespace-display-mappings
;;       '((space-mark ?\u3000 [?\u25a1])
;;         ;; WARNING: the mapping below has a problem.
;;         ;; When a TAB occupies exactly one column, it will display the
;;         ;; character ?\xBB at that column followed by a TAB which goes to
;;         ;; the next TAB column.
;;         ;; If this is a problem for you, please, comment the line below.
;;         ;; (space-mark ?\u0020 [?\xB7])  ; 半角スペース
;;         ;; (newline-mark ?\n   [?\u21B5 ?\n]) ; 改行記号
;;         (newline-mark ?\n   [?$ ?\n]) ; 改行記号
;;         ;; (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t]) ; タブマーク
;;         (tab-mark ?\t [?> ?\t]) ; タブマーク
;;         )
;;       )

;; ;;----------------------------------------------------------
;; ;; スペース（全角／半角）はを可視化
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
  (global-display-line-numbers-mode)
  (set-face-attribute 'line-number nil
                      :foreground "#aaa")
  (set-face-attribute 'line-number-current-line nil
                      :foreground "#fff")
)

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
(use-package find-file-in-project
  :ensure t
  )

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
  :config
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
  (ivy-mode 1)
  )

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1)
)


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
  :config
  ;; キーバインドは一例です．好みに変えましょう．
  (global-set-key (kbd "M-s M-s") 'swiper-thing-at-point)
  )


;;----------------------------------------------------------
;; シンボルハイライト
(use-package symbol-overlay
  :ensure t
  :config
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
  :config
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'ivy)
  )

;; ;;----------------------------------------------------------
;; ;; git-gutter+ git 変更分を表示
;; (use-package git-gutter+
;;   :ensure t
;;   )
;; (global-git-gutter+-mode t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

;;----------------------------------------------------------
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  )

;;----------------------------------------------------------
;; flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (when (locate-library "flycheck-irony")
    (flycheck-irony-setup))
  ;; (setq flycheck-checker-error-threshold 500)
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)
  )

;; ;; Flycheckをグローバルに有効にします
;; (global-flycheck-mode)
;; ;; ESLintをJavaScriptのチェッカーとして追加します
;; (flycheck-add-mode 'javascript-eslint 'vue-mode)
;; ;; Flycheckをvue-modeで有効にします
;; (add-hook 'vue-mode-hook 'flycheck-mode)

;; マウスをクリックしている間だけエラー表示
(defun my-mouse-click (event)
  (interactive "e")
  (mouse-set-point event)
  (when (bound-and-true-p flycheck-mode)
    (flycheck-display-error-at-point)
    )
  )
(global-set-key [down-mouse-1] 'my-mouse-click)

;; (defun my-mouse-click (event)
;;   (interactive "e")
;;   (mouse-set-point event)
;;   (when (bound-and-true-p flycheck-mode)
;;     (flycheck-display-error-at-point)
;;     (setq flycheck-display-errors-function
;;           #'flycheck-display-error-messages)))

;; (global-set-key [down-mouse-1] 'my-mouse-click)


;; ------------------------------------------------------------------------------
;; cc-mode
(use-package cc-mode
  :ensure t
  )


(use-package rbenv
  :ensure t
  :config
  (setq rbenv-installation-dir "~/.rbenv")
  )

;; ------------------------------------------------------------------------------
;; robe
;; robe に必要なgem
;;  $ gem install pry pry-nav pry-doc
;; 起動方法
;;  M-x: inf-ruby
;;  M-x; robe-start
(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'ac-robe-setup)
  (autoload 'robe-mode "robe" "Code navigation, documentation lookup and completion for Ruby" t nil)
  (eval-after-load 'company
    '(push 'company-robe company-backends))
  (add-hook 'ruby-mode-hook (lambda()
                              (company-mode)
                              (setq company-auto-expand t)
                              (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
                              (setq company-idle-delay 0) ; 遅延なしにすぐ表示
                              (setq company-minimum-prefix-length 1) ; 何文字打つと補完動作を行うか設定
                              (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
                              (setq completion-ignore-case t)
                              (setq company-dabbrev-downcase nil)
                              (global-set-key (kbd "C-M-i") 'company-complete)
                              ;; C-n, C-pで補完候補を次/前の候補を選択
                              (define-key company-active-map (kbd "C-n") 'company-select-next)
                              (define-key company-active-map (kbd "C-p") 'company-select-previous)
                              (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
                              (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
                              (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
                              ))
  )

;; ------------------------------------------------------------------------------
;; yasnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind (:map yas-minor-mode-map
              ("C-x i i" . yas-insert-snippet)
              ("C-x i n" . yas-new-snippet)
              ("C-x i v" . yas-visit-snippet-file)
              ("C-x i l" . yas-describe-tables)
              ("C-x i g" . yas-reload-all))
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-ido-prompt))
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
  :config
  ;; 補完が自動でポップアップしないように設定
  (setq company-idle-delay nil)
  ;; 補完候補の最大表示数
  (setq company-tooltip-limit 20)
  ;; ツールチップの表示位置を調整
  (setq company-tooltip-align-annotations t)
  ;; 全バッファで有効にする
  (global-company-mode t))

(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))

(use-package company-irony-c-headers
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony-c-headers))

(global-company-mode) ; 全バッファで有効にする

(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)



(defun indent-region-with-tabs ()
  "選択範囲をタブでインデントする"
  (interactive)
  (let ((indent-tabs-mode t))
    (indent-region (region-beginning) (region-end))))
(global-set-key (kbd "C-M-\\") 'indent-region-with-tabs) ;; キーバインドを設定


;;(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
;;(setq auto-mode-alist
;;      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


;; emacs29から取り込まれているらしい
;; (use-package csharp-mode
;;   :ensure t
;;   )
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
;; python 補完機能
;; 準備：$ pip install jedi flake8 importmagic autopep8 yapf black
;; M-x : elpy-config : elpy の状況確認画面が表示される
;; M-x : pyvenv-activate : プロジェクトルートディレクトリ選択
;; F1: 補完候補表示中に選択している要素のマニュアルを見る

;; ;; 以下は不要
;; ;; (el-get-bundle company-jedi :depends (company-mode))
;; (use-package company-jedi
;;   :ensure t
;;   )
;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))
;; (add-hook 'python-mode-hook 'my/python-mode-hook)

;; elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (setq elpy-rpc-virtualenv-path 'current)
  (add-hook 'elpy-mode-hook
    '(lambda ()
       ;; (auto-complete-mode -1)
       (define-key company-active-map (kbd "C-n") 'company-select-next)
       (define-key company-active-map (kbd "C-p") 'company-select-previous)
       (define-key company-active-map (kbd "<tab>") 'company-complete)))
)

;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable)
;;   :config
;;   ;; (remove-hook 'elpy-modules 'elpy-module-highlight-indentation) ;; インデントハイライトの無効化
;;   (remove-hook 'elpy-modules 'elpy-module-flymake) ;; flymakeの無効化
;;   (setq elpy-rpc-python-command "python3")
;;   (setq elpy-rpc-virtualenv-path 'current)
;;   (setq elpy-rpc-backend "jedi")
;;   ;; (setq jedi:complete-on-dot t)
;;   ;; (add-hook 'python-mode-hook 'jedi:setup)
;;   (add-hook 'elpy-mode-hook
;;     '(lambda ()
;;        ;; (auto-complete-mode -1)
;;        (define-key company-active-map (kbd "C-n") 'company-select-next)
;;        (define-key company-active-map (kbd "C-p") 'company-select-previous)
;;        (define-key company-active-map (kbd "<tab>") 'company-complete))))

(use-package py-autopep8
  :ensure t
  :config
  (setq py-autopep8-options '("--max-line-length=200"))
  (setq flycheck-flake8-maximum-line-length 200)
  (py-autopep8-mode)
  )

;; (custom-set-variables
;;  '(flycheck-python-flake8-executable "python3")
;;  '(flycheck-python-pycompile-executable "python3")
;;  '(flycheck-python-pylint-executable "python3"))

;; ------------------------------------------------------------------------------
;; js-mode
;;  # eslint をとりあえず global に導入
;;  $ npm install -g eslint  # グローバルに入れておけば全体で使えるけど、プロジェクト単位でもいい
;;  $ npx npx eslint --init  # 各プロジェクトに設定する
(add-hook 'js-mode-hook
          (lambda ()
            (flycheck-mode)
            (setq-local flycheck-checker 'javascript-eslint)
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)
            )
          )
(flycheck-add-mode 'javascript-eslint 'js-mode)

;; (use-package js2-mode
;;   :ensure t
;;   :mode (("\\.js\\'" . js2-mode))
;;   :config
;;   (setq js-indent-level 2)
;;   (setq js2-basic-offset 2)
;;   (setq js2-bounce-indent-p t)
;;   )

;; (flycheck-add-mode 'javascript-eslint 'js2-mode)

;; ------------------------------------------------------------------------------
;; jade-mode
(use-package jade-mode
  :ensure t
  :config
  )

;; ------------------------------------------------------------------------------
;; markdown
(use-package markdown-mode
  :ensure t
  :config
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  ;; コードブロックのハイライト化
  (setq markdown-fontify-code-blocks-natively t)
  )

;; typescript
(use-package tide
  :ensure t
  )
(use-package typescript-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-hook 'typescript-mode-hook
            '(lambda ()
               (interactive)
               (tide-setup)
               (flycheck-mode +1)
               (tide-hl-identifier-mode +1)
               (company-mode +1)
               (eldoc-mode +1)
               (setq typescript-indent-level 2)
               ))
  )

;; ------------------------------------------------------------------------------
;; yaml-mode
(use-package yaml-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  )

;; ------------------------------------------------------------------------------
;; csv-mode
(use-package csv-mode
  :ensure t
  )
;; csv-align-fields(C-c C-a)
;; csv-unalign-fields(C-c C-u)
;; (when (require 'csv-mode nil t)
;;   (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
;;   (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
;;   )

;; ------------------------------------------------------------------------------
;; web-mode(html)
;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-markup-indent-offset 2)
;; )

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  ;; (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#D0D0D0")
  ;; (set-face-attribute 'web-mode-doctype-face nil :foreground "#D0D0F0")
  ;; (add-hook 'web-mode-hook  'my-web-mode-hook)
  (setq web-mode-markup-indent-offset 2) ; HTMLタグのインデント
  (setq web-mode-css-indent-offset 2)    ; CSSのインデント
  (setq web-mode-code-indent-offset 2)  ; JS, PHP, Javaなどのインデント
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#d0d0d0")  ; html タグカッコを強制白色にする
  )

;; slim-modeの設定
(use-package slim-mode
  :ensure t
  :config
  (setq slim-indent-offset 2))


;; scss-modeの設定
(when (>= emacs-major-version 30)
  (setq flymake-allowed-file-name-masks
        '((".+\\.scss$" flymake-scss-init)))
  (setq flymake-err-line-patterns
        '(("on line \\([0-9]+\\) of \\([^ ]+\\)$" 2 1 nil 2))))

(use-package scss-mode
  :ensure t
  :config
  (setq scss-compile-at-save nil) ; 自動コンパイルを無効にする
  (setq css-indent-offset 2))     ; インデント幅を2スペースに設定


;; ------------------------------------------------------------------------------
;; vue-mode
;; (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))

;; (use-package vue-mode
;;   :ensure t
;;   :config
;;   (setq mmm-submode-decoration-level 0)
;;   (setq mmm-global-mode 'maybe)
;;   (setq vue-html-tab-width 2)
;;   (setq vue-css-tab-width 2)
;;   (setq css-indent-offset 2)
;;   (setq js-indent-level 2)
;;   ;; (add-hook 'vue-mode-hook
;;   ;;           (lambda ()
;;   ;;             (setq vue-html-tab-width 2)))
;;   ;; (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;;   ;; (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;;   ;; vue-modeでESLintを有効化する
;;   ;; (add-to-list 'vue-mode-hook #'smartparens-mode)
;;   ;; (require 'lsp-ui)
;;   ;; (require 'lsp-vue)
;;   ;; (add-hook 'vue-mode-hook #'lsp-vue-mmm-enable)
;;   ;; (with-eval-after-load 'lsp-ui (require 'lsp-ui-flycheck))
;;   ;; (require 'company-lsp)
;;   ;; (push 'company-lsp company-backends)
;;   (flycheck-add-mode 'javascript-eslint 'vue-mode)
;;   (add-hook 'vue-mode-hook 'flycheck-mode)
;;   )

;; 以下の設定の場合、typescript, vls が必要なのでインストールしておくこと
;; $ npm install -g typescript vls
;; M-x: lsp-workspace-folders-add でvueプロジェクトルートを指定する(ただしvueファイルを一旦オープンしておく必要がある様子。そのあとリオープンしないとダメっぽい）
(use-package vue-mode
  :ensure t
  :config
  (setq mmm-submode-decoration-level 0)
  (setq mmm-global-mode 'maybe)
  (setq vue-html-tab-width 2)
  (setq vue-css-tab-width 2)
  (setq css-indent-offset 2)
  (setq js-indent-level 2)
  (flycheck-add-mode 'javascript-eslint 'vue-mode)
  (add-hook 'vue-mode-hook 'flycheck-mode))

(defvar my-tsc-path
  (cond
   ((eq system-type 'windows-nt)
    "C:\\Users\\大家一訓\\AppData\\Local\\fnm_multishells\\22284_1744770763216\\tsc")  ;; Windowsのパス
   ((eq system-type 'gnu/linux)
    "/home/developer/.anyenv/envs/nodenv/shims/tsc")  ;; Linuxのパス
   ((eq system-type 'darwin)
    "/Users/developer/.anyenv/envs/nodenv/shims/tsc"))) ;; macOSのパス

(use-package lsp-mode
  :ensure t
  :hook (vue-mode . lsp)
  :commands lsp
  :config
  (setq lsp-clients-typescript-server "volar")
  (setq lsp-auto-guess-root t)
  (setq lsp-volar-take-over-mode t)
  (setq lsp-volar-typescript-server-path my-tsc-path)) ;; 変数を使用(TypeScriptのパスを設定)

;; (use-package lsp-mode
;;   :ensure t
;;   :hook (vue-mode . lsp)
;;   :commands lsp
;;   :config
;;   (setq lsp-clients-typescript-server "volar")
;;   (setq lsp-auto-guess-root t)
;;   (setq lsp-volar-take-over-mode t)
;;   (setq lsp-volar-typescript-server-path "/home/developer/.anyenv/envs/nodenv/shims/tsc"))  ;; TypeScriptのパスを設定

;; (use-package company
;;   :ensure t
;;   :config
;;   (setq company-tooltip-align-annotations t)
;;   (setq company-tooltip-limit 20)
;;   (global-company-mode))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(setq completion-styles '(basic substring))


;; ;; ************************************************************************
;; ;; mmm-mode
;; ;; ************************************************************************
;; (use-package mmm-mode
;;   :ensure t
;;   :config
;;   (setq mmm-global-mode 'maybe)
;;   (mmm-add-classes
;;    '((vue-embeded-slim-mode
;;       :submode slim-mode
;;       :front "^<template.*lang=\"pug\">\n"
;;       :back "^</template>")
;;      (vue-embeded-web-mode
;;       :submode web-mode
;;       :front "^<template>\n"
;;       :back "^</template>\n")
;;      (vue-js-mode
;;        :submode js-mode
;;        :front "<script[^>]*>[ \t]*\n"
;;        :back "[ \t]*</script>")
;;      ;; (vue-embeded-js-mode
;;      ;;  :submode js-mode
;;      ;;  :front "^<script.*>\n"
;;      ;;  :back "^</script>\n")
;;      ;; (vue-embeded-js-setup-mode
;;      ;;  :submode js-mode
;;      ;;  :front "^<script setup>\n"
;;      ;;  :back "^</script>\n")  ; 新たに追加しました
;;      (vue-embeded-scss-mode
;;       :submode scss-mode
;;       :front "^<style.*lang=\"scss\">\n"
;;       :back "^</style>")
;;      (vue-embeded-css-mode
;;       :submode web-mode
;;       :front "^<style>\n"
;;       :back "^</style>")
;;      ))
;;   (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-slim-mode)
;;   (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-web-mode)
;;   (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-js-mode)
;;   ;; (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-js-mode)
;;   ;; (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-js-setup-mode)  ; 新たに追加しました
;;   (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-scss-mode)
;;   (mmm-add-mode-ext-class nil "\\.vue\\'" 'vue-embeded-css-mode)
;;   )

;; (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
;; (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))

;; ------------------------------------------------------------------------------
;; Meadow時代のofficeファイルをテキスト化するパッケージ（独自改良版）
(use-package xdoc2txt
  :load-path "~/.emacs.d/elisp"
  )

(setq require-final-newline nil)

;; ;;----------------------------------------------------------
;; ;; 終了時のバッファ内容を保持
;; (desktop-save-mode t)

;;--- end of my settings ---
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
