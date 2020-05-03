(message "OS is %s." system-type)
(message "HOME is %s." (getenv "HOME"))
(message "PATH is %s." (getenv "PATH"))
(setenv "LC_MESSAGES" "C")

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
;; パス指定
(setenv "PATH"
        (concat
         "C:\\msys64\\mingw64\\bin" ";"
         (getenv "PATH")))

(setq exec-path (append exec-path '("c:/msys64/mingw64/bin")))

;;----------------------------------------------------------
;; 文字コード指定
(set-language-environment "Japanese")
(when (eq system-type 'windows-nt)
  (set-default-coding-systems 'utf-8-unix) ; デフォルトの文字コード
  (prefer-coding-system 'utf-8-unix)
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
(setq ns-pop-up-frames nil)

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
;; ウィンドウ移動
;; shift ↑↓←→
(setq windmove-wrap-around t)
(windmove-default-keybindings)

;;----------------------------------------------------------
;; ;; 勝手にウィンドウを分割するのをやめる
;; (setq split-width-threshold nil)
;; (setq split-height-threshold nil)

;;----------------------------------------------------------
;; タイトルバーに編集中のファイルのパス名を表示
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;;----------------------------------------------------------
;; 移動が遅くなる問題を回避
(setq auto-window-vscroll nil)

;;----------------------------------------------------------
;; powerline フォント
(when (eq system-type 'windows-nt)
  (set-face-font 'default "Ricty Diminished for Powerline-12")
  )

;;----------------------------------------------------------
;; 行番号
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

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

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile)
  )

(setq omn​​isharp-company-strip-trailing-brackets nil)
(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

;;--- end of my settings ---
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-mode yasnippet use-package omnisharp dracula-theme company-irony-c-headers company-irony))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
