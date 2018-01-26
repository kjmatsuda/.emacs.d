;; 文字コード
(prefer-coding-system 'utf-8)

(if (win?)
    (progn
      (set-file-name-coding-system 'cp932)

      ;; 参考  http://skalldan.wordpress.com/2011/11/09/ntemacs-%E3%81%A7-utf-8-%E3%81%AA%E7%92%B0%E5%A2%83%E6%A7%8B%E7%AF%89%E3%82%92%E8%A9%A6%E8%A1%8C%E9%8C%AF%E8%AA%A4/
      ;; Setenv
      (setenv "LANG" "ja_JP.UTF-8")

      ;; 言語環境
      (set-language-environment "Japanese")

      ;; 文字コード
      (set-buffer-file-coding-system 'utf-8-unix)
      (set-terminal-coding-system 'utf-8-unix)
      (setq default-buffer-file-coding-system 'utf-8-unix)
      ;; デフォルトの文字コード
      (set-default-coding-systems 'utf-8-unix)
      ;; 新規バッファの文字コード
      (prefer-coding-system 'utf-8-unix)
      ;; ファイル名の文字コード
      (set-file-name-coding-system 'utf-8-unix)
      ;; キーボード入力の文字コード
      (set-keyboard-coding-system 'utf-8-unix)      

      ;; Shell Mode
      (setq shell-mode-hook
        (function (lambda()
            (set-buffer-process-coding-system 'utf-8-unix
                              'utf-8-unix))))

      ;; Grep
      (defadvice grep (around grep-coding-setup activate)
    (let ((coding-system-for-read 'utf-8))
      ad-do-it))))

;;;;;;;;;;;;;;;;;  画面の基本設定 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if window-system (progn
  ;; 文字の色を設定
  (add-to-list 'default-frame-alist '(foreground-color . "green"))
  ;; フォントの設定
 (if (linux?)
     ;; プログラミング用フォント Ricty
     (add-to-list 'default-frame-alist '(font . "ricty-12")))
    ;;  (add-to-list 'default-frame-alist '(font . "-unknown-VL ゴシック-normal-normal-normal-*-13-*-*-*-*-0-iso10646-1"))

  ;; 背景色を設定します。
  (add-to-list 'default-frame-alist '(background-color . "black"))
  ;; 背景の透過
  (add-to-list 'default-frame-alist '(alpha . (1.00 1.00)))
  (defun set-frame-parameter-alpha (alpha)
    "set frame parameter to argument alpha"
    (interactive "nInput alpha value(0~100):")
    (set-frame-parameter nil 'alpha alpha))
  (global-set-key (kbd "C-c C-a") 'set-frame-parameter-alpha)

  ;; カ-ソルの色を設定します。
  (add-to-list 'default-frame-alist '(cursor-color . "SlateBlue2"))
  ;; マウスポインタの色を設定します。
  (add-to-list 'default-frame-alist '(mouse-color . "SlateBlue2"))
  (if (linux?)
      (add-to-list 'default-frame-alist '(width . 160))
    (add-to-list 'default-frame-alist '(width . 140)))
  (if (linux?)
      (add-to-list 'default-frame-alist '(height . 40))
    (add-to-list 'default-frame-alist '(height . 30)))
  ;; モ-ドライン（アクティブでないバッファ）の文字色を設定します。
  (set-face-foreground 'mode-line-inactive "gray30")
  ;; モ-ドライン（アクティブでないバッファ）の背景色を設定します。
  (set-face-background 'mode-line-inactive "gray85")
))

;; emacs 24.3 になってエラーになってしまった
;;;;モ-ド行の背景, 文字の色を変更
(set-face-background 'mode-line "grey10")
(set-face-foreground 'mode-line "SkyBlue")
(set-face-background 'highlight "grey10")
(set-face-foreground 'highlight "red")

;; スクロールした際のカーソルの移動行数
(setq scroll-conservatively 1)
;; 1 画面スクロール時にカーソルの画面上の位置をなるべく変えない
(setq scroll-preserve-screen-position t)

;; ツールバーは非表示
(tool-bar-mode -1)

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; リージョンに色を付ける
(setq transient-mark-mode t)

;; モードラインに桁数を表示
(column-number-mode 1)

;; モードラインにファイルサイズ表示
(size-indication-mode 1)

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;; 全角スペース/タブ文字を可視化
(setq whitespace-style
      '(tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        ))
(require 'whitespace)
(global-whitespace-mode 1)
(set-face-foreground 'whitespace-space "LightSlateGray")
(set-face-background 'whitespace-space "DarkSlateGray")
(set-face-foreground 'whitespace-tab "LightSlateGray")
(set-face-background 'whitespace-tab "DarkSlateGray")

;; 行番号を表示
; org-mode 時はlinumオンになると挙動が遅くなるので無効にしておく
(defadvice linum-on (around my-linum-on () activate)
  (unless (and (fboundp 'org-mode-p) (org-mode-p))
    ad-do-it))

;; org-modeで表を編集中にskkの漢字変換でEnterを押すと、
;; org-enterが走ってしまい、表が崩れる問題への対応
;; 参考
;; http://mail.ring.gr.jp/skk/201401/msg00001.html
(defadvice org-return (around skk-in-org-table activate)
  "org-modeの表の中でもskkが使えるようにする."
  (cond ((and (org-at-table-p) (not (null skk-henkan-mode)))
         (skk-kakutei))
        (t
         ad-do-it)))

;; (global-linum-mode t)
(setq linum-format "%4d|")


(setq-default tab-width 4 indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                      64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;; Windows専用設定
(if (win?)
    (progn
      ;; 日本語フォント設定
      (cond
       (window-system
        ;; (set-default-font "Courier New-11")
        (set-default-font "Ricty Diminished-12")
          ;; Japanese(japanese-jisx0208)font
          (set-fontset-font
           (frame-parameter nil 'font)
           'japanese-jisx0208
           (font-spec :family "Meiryo"))
    (set-fontset-font "fontset-default"
                  'katakana-jisx0201
                  '("ＭＳ ゴシック" . "jisx0201-katakana"))
    ))
      ;; フレームの最大化、切り替え
      (defvar w32-window-state nil)

      (defun w32-fullscreen-switch-frame ()
    (interactive)
    (setq w32-window-state (not w32-window-state))
    (if w32-window-state
        (w32-fullscreen-restore-frame)
      (w32-fullscreen-maximize-frame)
      ))

      (defun w32-fullscreen-maximize-frame ()
    "Maximize the current frame (windows only)"
    (interactive)
    (w32-send-sys-command 61488))

      (defun w32-fullscreen-restore-frame ()
    "Restore a minimized/maximized frame (windows only)"
    (interactive)
    (w32-send-sys-command 61728))

      (global-set-key [f11] 'w32-fullscreen-switch-frame)

      ;; cygwinのbash使用
      (setq explicit-shell-file-name "c:\\cygwin\\bin\\bash.exe")
      (modify-coding-system-alist 'process "shell" '(undecided-dos . sjis-unix))

      ;;; cygwin の find を使う
      ;;; あらかじめ findcyg.exe に rename しておく
      ;; 参考 http://antoine.st/MeadowSettings.html
      ;; cons の第2引数はカーソルの初期位置(''の間にくるよう設定)
      (setq grep-find-command
        (cons (concat "findcyg ./ -type f -name '*'"
              " | xargs grep -n -e '' {} nul \\;")
          50))
))

;; Linux用設定
(if (linux?)
    (progn
      ;; http://d.hatena.ne.jp/khiker/20090711/emacsfullscreen
      ;;上記リンクから引用
      (defun my-fullscreen ()
    (interactive)
    (let ((fullscreen (frame-parameter (selected-frame) 'fullscreen)))
      (cond
       ((null fullscreen)
        (set-frame-parameter (selected-frame) 'fullscreen 'fullboth))
       (t
        (set-frame-parameter (selected-frame) 'fullscreen 'nil))))
    (redisplay))

      (global-set-key [f11] 'my-fullscreen)
      (setq grep-find-command
        (cons (concat "find . -type f -print0"
              " | ""xargs"" -0 -e grep -nH -e ''")
          51))))

;; サイズ表示が 69913580 から 67M といったようにちょっと分かりやすくなる
(setq dired-listing-switches "-alh")

;; 現在行をハイライト表示する
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background  "#98FB98"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

;; imenu
(with-eval-after-load "imenu-list"
  (define-key imenu-list-major-mode-map (kbd "j") 'next-line)
  (define-key imenu-list-major-mode-map (kbd "k") 'previous-line))

(custom-set-variables '(imenu-list-size 0.2))

;; シンボルのハイライト表示
(require 'highlight-symbol)
(global-set-key [(control f3)] 'highlight-symbol-at-point)
(global-set-key (kbd "C-.") 'highlight-symbol-next)
(global-set-key (kbd "C-,") 'highlight-symbol-prev)
(global-set-key (kbd "M-R") 'highlight-symbol-query-replace)
