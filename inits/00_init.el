; melpa.el
(require 'melpa)

;; emacs 設定ファイル
(require 'cl)
;;;;;;;;;;;;;;;;;; 初期処理 ;;;;;;;;;;;;;;;;;;;;;;;;
;; OSのタイプを格納
(defvar os-type nil)

(cond ((string-match "apple-darwin" system-configuration) ;; Mac
       (setq os-type 'mac))
      ((string-match "linux" system-configuration)        ;; Linux
       (setq os-type 'linux))
      ((string-match "freebsd" system-configuration)      ;; FreeBSD
       (setq os-type 'bsd))
      ((string-match "mingw" system-configuration)        ;; Windows
       (setq os-type 'win)))

;; OSのタイプを判別する
(defun mac? ()
  (eq os-type 'mac))

(defun linux? ()
  (eq os-type 'linux))

(defun bsd? ()
  (eq os-type 'freebsd))

(defun win? ()
  (eq os-type 'win))

;; load-pathをサブディレクトリごと追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; elispとconfディレクトリをサブディレクトリごとload-pathに追加
(add-to-load-path "elisp" "conf")

(when (win?)
  (add-to-list 'load-path "C:/opt/emacs/site-lisp/apel")
  (add-to-list 'load-path "C:/opt/emacs/site-lisp/emu")
  (setq org-mobile-checksum-binary "~/.emacs.d/other/sha1sum.exe"))

;; http://pastelwill.jp/wiki/doku.php?id=emacs:org-mode
;; org-mode の開発版、安定板を切り替える
(setq load-path (append '(
              "~/Dropbox/.emacs.d/public_repos/org-mode/org-9.0.7/lisp"
              "~/Dropbox/.emacs.d/public_repos/org-mode/org-9.0.7/contrib/lisp"
			  ;; "~/Dropbox/.emacs.d/public_repos/org-mode/head/lisp"       ; The latest org-mode
              ;; "~/Dropbox/.emacs.d/public_repos/org-mode/head/contrib/lisp"
              ;; "~/Dropbox/.emacs.d/public_repos/org-mode/org-8.2.2/lisp"
              ;; "~/Dropbox/.emacs.d/public_repos/org-mode/org-8.2.2/contrib/lisp"
			  ) load-path))

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
      (set-buffer-file-coding-system 'utf-8)
      (set-terminal-coding-system 'utf-8)
      (set-keyboard-coding-system 'utf-8)
      (setq default-buffer-file-coding-system 'utf-8)
      (set-selection-coding-system 'utf-16le-dos)

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


;; アイドルにしてたらマトリックスのような表示
;; マトリックス以外の zone は削除
(setq zone-programs nil)
(require 'zone-matrix)
(require 'zone-matrix-settings)
(require 'zone-settings)

(setq-default tab-width 4 indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60
                      64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;; 行末の空白を削除する
(global-set-key (kbd "C-x C-s") 'delete-trailing-whitespace)

;; Windows専用設定
(if (win?)
    (progn
      ;; 日本語フォント設定
      (cond
       (window-system
        (set-default-font "Courier New-11")
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


;; フレームサイズ切替
;; (load-file "~/.emacs.d/util/my-screen.el")
;; (global-set-key [f11] 'my-fullscreen)

;; フレームの操作性を向上する
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

;; C-S-oで次のwindowへカーソルを移す
(global-set-key (kbd "C-S-o") 'other-window-or-split)

(defun select-buffer-start-with (b-list buff-name-top)
  (if (eq b-list nil)
      nil
    (if (eq (string-match (concat "^" buff-name-top) (buffer-name (car b-list))) 0)
        ;; バッファ名が一致した
        (switch-to-buffer (car b-list))
      (select-buffer-start-with (cdr b-list) buff-name-top))))

;; clojure開発用にウィンドウを構成する
(defun window-manage-for-develop ()
  (interactive)
  (delete-other-windows)
  (neotree)
  (other-window 1)
  (split-window-vertically (/ (* 2 (window-height)) 3))
  (other-window 1)
  (if (eq major-mode 'clojure-mode)
      (select-buffer-start-with (buffer-list) "*cider-repl")
    (if (eq major-mode 'lisp-mode)
        (select-buffer-start-with (buffer-list) "*slime-repl")))
  (other-window 1)
  (other-window 1))

;; 画面左にディレクトリツリーを表示
(require 'neotree)
(global-set-key (kbd "M-+") 'window-manage-for-develop)
(global-set-key (kbd "M--") 'neotree-hide)

;; cua-mode の設定
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; 'pop-mark' C-u C-SPC C-SPC C-SPC... のように
;; C-SPC を連続で入力するだけで，連続でマークを辿れるようになる
(setq set-mark-command-repeat-pop t)

;; ;; バックアップとオートセーブファイルを保存する
;; (add-to-list 'backup-directory-alist
;; 	  (cons "." "~/Dropbox/backups/"))
;; (setq auto-save-file-name-transforms
;;       `((".*" ,(expand-file-name "~/Dropbox/backups/") t)))
;; (setq auto-save-intarval 60)

;; バックアップファイルを作成しない
(setq make-backup-files nil)
;; オートセーブファイルを作らない
(setq auto-save-default nil)

;; (global-set-key "\M-g" 'goto-line)
;; (global-set-key (kbd "C-M-g") 'igrep)
(global-set-key (kbd "C-x g") 'grep-find)
(global-set-key (kbd "C-M-e") 'ediff-merge-files)
;; (global-set-key "\M-o" 'occur-by-moccur)
;; (global-set-key (kbd "C-M-o") 'moccur)
(global-set-key (kbd "C-x C-o") 'moccur-grep-find)
;; (global-set-key [\C-\tab] 'dabbrev-expand)
(global-set-key (kbd "C-c C-i") 'indent-region)
(global-set-key (kbd "C->") 'comment-region)
(global-set-key (kbd "C-<") 'uncomment-region)
;; 改行と同時にインデント
(global-set-key (kbd "C-m") 'newline-and-indent)
;; C-h でバックスペース
(keyboard-translate ?\C-h ?\C-?) ; C-h -> BS
(global-set-key (kbd "C-H") 'help-command)
;; ;; 文字の拡大、縮小、元に戻す
;; (global-set-key (kbd "C-M-;") (lambda () (interactive) (text-scale-increase 1)))
;; ;; 文字の縮小
;; (global-set-key (kbd "C-M--") (lambda () (interactive) (text-scale-decrease 1)))
;; 文字のサイズを元に戻す
(global-set-key (kbd "C-M-0") (lambda () (interactive) (text-scale-increase 0)))

;; http://www.gentei.org/~yuuji/software/euc/instamp.el
;; 現在時刻挿入
(autoload 'instamp
  "instamp" "Insert TimeStamp on the point" t)
(define-key global-map "\M-s" 'instamp)
(setq instamp-date-format-list-private
      '("%Y%m%d"))

;; M-wやC-kでコピーしたものを、他のアプルケーションで貼り付け可能にする
(cond (window-system
       (setq x-select-enable-clipboard t)
))

;; 対応する括弧等を自動挿入する。以下を評価してインストール
;; (auto-install-from-url "https://github.com/uk-ar/skeleton-pair-dwim/raw/master/skeleton-pair-dwim.el")
;; (require 'skeleton-pair-dwim)
;; (skeleton-pair-dwim-load-default)
;; ;; < >でエラーが出るためキーバインドを再定義
;; (define-key (current-global-map) (kbd "<") 'self-insert-command)
;; (define-key (current-global-map) (kbd ">") 'self-insert-command)
;; (define-key (current-global-map) (kbd "`") 'self-insert-command)
;; (define-key (current-global-map) (kbd "'") 'self-insert-command)
;; (skeleton-pair-dwim-global-set-key '("{" "(" "\"" "'" "`" "<" "[") 'self-insert-command);;unload default
;; (skeleton-pair-dwim-define-key
;; '(global-map lisp-mode-map) '("{" "\"" "[") 'skeleton-pair-insert-dwim)

;; 参考
;; http://d.hatena.ne.jp/uk-ar/20111208/1322572618%3E
(require 'key-combo)
(key-combo-load-default)
(key-combo-define-global (kbd "(") "(`!!')")
(key-combo-define-global (kbd "\"") "\"`!!'\"")
(key-combo-define-global (kbd "[") "[`!!']")
(key-combo-define-global (kbd "{") "{`!!'}")
(key-combo-define-global (kbd "// ") "// ")
;; 古い設定2013/11/22時点
;; (key-combo-define-global (kbd "(") '("(`!!')"))
;; (key-combo-define-global (kbd "()") "()")
;; (key-combo-define-global (kbd "((") "((`!!'))")
;; (key-combo-define-global (kbd "\"") '("\"`!!'\""))
;; (key-combo-define-global (kbd "\"\"") "\"\"")
;; (key-combo-define-global (kbd "{") '("{`!!'}"))
;; (key-combo-define-global (kbd "{}") "{}")
;; (key-combo-define-global (kbd "[") '("[`!!']"))
;; (key-combo-define-global (kbd "[]") "[]")

;; 英和辞書
(when (require 'sdic nil t)
  (global-set-key "\C-cw" 'sdic-describe-word)
  (global-set-key "\C-cp" 'sdic-describe-word-at-point))
;; 動作と見掛けを調節するための設定
(setq sdic-window-height 10
      sdic-disable-select-window t)
(if (win?)
    (progn
      ;; 使用する辞書ファイルの設定
      (setq sdic-eiwa-dictionary-list '((sdicf-client "~/Dropbox/.emacs.d/dict/gene.sdic")))
      (setq sdic-waei-dictionary-list '((sdicf-client "~/Dropbox/.emacs.d/dict/jedict.sdic")))
      )
  )
;; 単語の意味をツールチップで表示する
(defun temp-cancel-read-only (function &optional jaspace-off)
  "eval temporarily cancel buffer-read-only
&optional t is turn of jaspace-mode"
  (let ((read-only-p nil)
        (jaspace-mode-p nil))
    (when (and jaspace-off jaspace-mode)
      (jaspace-mode)
      (setq jaspace-mode-p t))
    (when buffer-read-only
      (toggle-read-only)
      (setq read-only-p t))
    (eval function)
    (when read-only-p
      (toggle-read-only))
    (when jaspace-mode-p
      (jaspace-mode))))

(defun my-sdic-describe-word-with-popup (word &optional search-function)
  "Display the meaning of word."
  (interactive
   (let ((f (if current-prefix-arg (sdic-select-search-function)))
         (w (sdic-read-from-minibuffer)))
     (list w f)))
  (let ((old-buf (current-buffer))
        (dict-data))
    (set-buffer (get-buffer-create sdic-buffer-name))
    (or (string= mode-name sdic-mode-name) (sdic-mode))
    (erase-buffer)
    (let ((case-fold-search t)
          (sdic-buffer-start-point (point-min)))
      (if (prog1 (funcall (or search-function
                              (if (string-match "\\cj" word)
                                  'sdic-search-waei-dictionary
                                'sdic-search-eiwa-dictionary))
                          word)
            (set-buffer-modified-p nil)
            (setq dict-data (buffer-string))
            (set-buffer old-buf))
          (temp-cancel-read-only
           '(popup-tip dict-data :scroll-bar t :truncate nil))
        (message "Can't find word, \"%s\"." word))))
  )

(defadvice sdic-describe-word-at-point (around sdic-popup-advice activate)
  (letf (((symbol-function 'sdic-describe-word) (symbol-function 'my-sdic-describe-word-with-popup)))
    ad-do-it))

;; wdired
;; http://at-aka.blogspot.com/2006/12/emacs-dired-wdired.html
;; (eval-after-load "dired"
;;   '(lambda ()
;;      (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))
(require 'dired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; ワンキーで dired のソートタイプを切り替える
;; 参考 http://d.hatena.ne.jp/mooz/20091207/p1
;; "s" で順送り切り替え
;; "c" でワンクリック切り替え
(defvar dired-various-sort-type
  '(("S" . "size")
    ("X" . "extension")
    ("v" . "version")
    ("t" . "date")
    (""  . "name")))

(defun dired-various-sort-change (sort-type-alist &optional prior-pair)
  (when (eq major-mode 'dired-mode)
    (let* (case-fold-search
           get-next
           (options
            (mapconcat 'car sort-type-alist ""))
           (opt-desc-pair
            (or prior-pair
                (catch 'found
                  (dolist (pair sort-type-alist)
                    (when get-next
                      (throw 'found pair))
                    (setq get-next (string-match (car pair) dired-actual-switches)))
                  (car sort-type-alist)))))
      (setq dired-actual-switches
            (concat "-l" (dired-replace-in-string (concat "[l" options "-]")
                                                  ""
                                                  dired-actual-switches)
                    (car opt-desc-pair)))
      (setq mode-name
            (concat "Dired by " (cdr opt-desc-pair)))
      (force-mode-line-update)
      (revert-buffer))))

(defun dired-various-sort-change-or-edit (&optional arg)
  "Hehe"
  (interactive "P")
  (when dired-sort-inhibit
    (error "Cannot sort this dired buffer"))
  (if arg
      (dired-sort-other
       (read-string "ls switches (must contain -l): " dired-actual-switches))
    (dired-various-sort-change dired-various-sort-type)))

(defvar anything-c-source-dired-various-sort
  '((name . "Dired various sort type")
    (candidates . (lambda ()
                    (mapcar (lambda (x)
                              (cons (concat (cdr x) " (" (car x) ")") x))
                            dired-various-sort-type)))
    (action . (("Set sort type" . (lambda (candidate)
                                    (dired-various-sort-change dired-various-sort-type candidate)))))
    ))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "s" 'dired-various-sort-change-or-edit)
             (define-key dired-mode-map "c"
               '(lambda ()
                  (interactive)
                  (anything '(anything-c-source-dired-various-sort))))
             ))

;; サイズ表示が 69913580 から 67M といったようにちょっと分かりやすくなる
(setq dired-listing-switches "-alh")

;; ;; Visual Basicモード
;; (auto-install-from-url "http://www.emacswiki.org/emacs/download/visual-basic-mode.el")
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|FRM\\|BAS\\|bas\\|cls\\|vb\\)$" .
                                 visual-basic-mode)) auto-mode-alist))
;; コードの折り畳み
(add-hook 'visual-basic-mode-hook
          '(lambda ()
    (hs-minor-mode 1)))

;; file名の補完で大文字小文字を区別しない
(setq completion-ignore-case t)

;; バッファ自動再読み込み
(global-auto-revert-mode 1)

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

;;graphviz mode
(load "graphviz-dot-mode.el")
(add-hook 'graphviz-dot-mode-hook (lambda () (local-set-key [f6] "\C-cc\C-m\C-cp")))

;;;;;;;;;;;;;;;;;  Emacsテクニックバイブルより ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14. org-mode
(require 'org)

;; 14.4 M-x org-remember
;; (org-remember-insinuate) ;; org-mode を最新にしたらエラーが出たのでコメントアウト2013/11/8
(setq org-directory "~/Dropbox/org/")
(setq org-mobile-directory "~/Dropbox/mobileorg/")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/goals.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/dictionary.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/idea.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/knowledge_of_failure.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/main.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/project_business_client.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/project_business_private.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/project_company.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/project_free_private.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/project_volunteer_client.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/server.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/study.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/taiko.org")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org/tips.org")

;; (setq org-remember-templates
;;       '(("Memo" ?m "** %?\n %i %a\n %t" nil "Inbox")
;; 	;; ("Todo" ?t "** TODO %?\n %i %a\n %t" nil "Inbox")
;; 	("Twitter" ?t "** %?\n %i %t" nil "Twitter")))
;; (global-set-key (kbd "C-,") 'org-remember)


(setq org-default-notes-file (expand-file-name "capture/capture.org" org-directory))
(require 'org-capture)
(setq org-capture-templates
      '(("t" "Task" entry (file+headline nil "Inbox")
         "** TODO %?\n %T\n %a\n %i\n")
        ("b" "Bug" entry (file+headline nil "Inbox")
         "** TODO %?   :bug:\n  %T\n %a\n %i\n")
        ("i" "Idea" entry (file+headline nil "Idea")
         "** %?\n %U\n %i\n %a\n %i\n")
        ("c" "Comment" entry (file+headline nil "Code Reading")
         "** %?\n %U\n %i\n %a\n %i\n")
        ("w" "Twitter" entry (file+datetree "twitter.org")
         "** %U %?\n")
        )
      )
(global-set-key (kbd "C-,") 'org-capture)


;; 14.6 TODOリストを作成する
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCEL(c)")))

;; 14.14 予定表を見る
;; アジェンダ表示の対象ファイル
(setq org-agenda-files (list org-directory))

(global-set-key (kbd "C-c a") 'org-agenda)
;; mobileorg のアジェンダ表示を改善するべく、org-agenda-custom-commands を触った。2015/3/26
;; どこかが悪さをして、orgmobile から google カレンダーへの同期が効かなくなったので
;; いくつかコメントアウトした。
;; どれがまずかったのかまでは分からない。org-agenda-sorting-strategy,org-deadline-warning-days
(setq org-agenda-custom-commands
      '(
        ;; ("d" "全般：今日すること"
        ;;  ((agenda "" ((org-agenda-ndays 1)
        ;;               (org-deadline-warning-days 1) ;; 締切日の一日前のアジェンダに表示
        ;;               ;; (org-agenda-sorting-strategy
        ;;               ;;  (quote ((agenda time-up priority-down tag-up))))
        ;;               ;; (org-deadline-warning-days 0)
        ;;               ))))
        ;; ("w" "全般：今週すること"
        ;;  ((agenda "" ((org-agenda-ndays 7)
        ;;               (org-deadline-warning-days 7)
        ;;               ;; (org-agenda-sorting-strategy
        ;;               ;;  (quote ((agenda time-up priority-down tag-up))))
        ;;               ;; (org-deadline-warning-days 0)
        ;;               ))))
        ;; ("g" "目標"
        ;;  ((tags-todo "goals")))
        ("h" "目標：今日すること" ((agenda "" ((org-agenda-ndays 1)
                                               (org-deadline-warning-days 1)
                                                ;; (org-agenda-sorting-strategy
                                                ;;  (quote ((agenda time-up priority-down tag-up))))
                                                ;; (org-deadline-warning-days 0)
                                                )))
         ((org-agenda-files '("~/Dropbox/org/goals.org" "~/Dropbox/org/projects.org"))
          ;; (org-agenda-sorting-strategy '(priority-up effort-down))
          )
         ;; ("~/computer.html")
         )
        ("j" "目標：今週すること" ((agenda "" ((org-agenda-ndays 7)
                                               (org-deadline-warning-days 7)
                                                ;; (org-agenda-sorting-strategy
                                                ;;  (quote ((agenda time-up priority-down tag-up))))
                                                ;; (org-deadline-warning-days 0)
                                                )))
         ((org-agenda-files '("~/Dropbox/org/goals.org" "~/Dropbox/org/projects.org"))
          ;; (org-agenda-sorting-strategy '(priority-up effort-down))
          )
         ;; ("~/computer.html")
         )
        ))

;; Googleカレンダーへエスクポート
;; ネタ元
;; http://d.hatena.ne.jp/t0m0_tomo/20100103/1262537012
(setq org-combined-agenda-icalendar-file "~/Dropbox/calendar/org.ics")
(setq org-icalendar-include-todo t)
;; (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))
;; (setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(event-if-todo))
(setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))

;; ;; org ファイルを google カレンダーに同期する
;; (defun start-process-org ()
;;   (interactive)
;;   (start-process-shell-command "org-sync-gcal" "*org-sync-gcal*" "emacs" "--script" "~/Dropbox/.emacs.d/elisp/org-sync-gcal.el"))
;; (define-key global-map [f12] 'start-process-org)


(if (linux?)
    (progn
      ;; (start-process-org)
      ;; Emacs 終了時に mobileorg に push
      ;; (add-hook 'kill-emacs-hook 'org-mobile-push)
      (add-hook 'kill-emacs-hook
                '(lambda nil
                   (shell-command "set_goals_score")))))

;; サブタスクが残っているときに親タスクをDONEにできないようにする
(setq org-enforce-todo-dependencies t)

;; サブタスクが全て DONE になったら親タスクも自動的に DONE になり
;; サブタスクをひとつでも TODO にしたら 親タスクも TODO になる
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; カーソル位置のステータスを取得する関数
;; 返り値 :item, :headline, :headline-stars
(defun org-position-status(context-list)
  (if (not (cdr context-list))
      ;; context-list の末尾に到達
      (caar context-list)
    (org-position-status (cdr context-list))))

(add-hook
 'org-mode-hook
 (lambda ()
   (local-set-key (kbd "<M-S-return>") (lambda () (interactive)
                                       (if (not (eq (org-position-status (org-context)) :item))
                                           (progn
                                             (org-insert-heading-after-current))
                                         (org-insert-heading))))
   (local-set-key (kbd "<M-return>") (lambda () (interactive)
                                         (org-insert-todo-heading-respect-content)
                                         (insert (format "%s" "[0/1] "))))
   ;; 自作の M-return の動きに干渉するので key-combo-mode をオフにする
   (key-combo-mode -1)
   (auto-complete-mode t)
   ))

;; elscreen で<C-tab>をタブの切り換えに割り当てたいので無効にする
(define-key org-mode-map [(control tab)] nil)
(define-key org-mode-map (kbd "<M-S-return>") (lambda () (interactive)
                                       (if (not (eq (org-position-status (org-context)) :item))
                                           (progn
                                             (org-insert-heading-after-current))
                                         (org-insert-heading))))
(define-key org-mode-map (kbd "<M-return>") (lambda () (interactive)
                                         (org-insert-todo-heading-respect-content)
                                         (insert (format "%s" "[0/1] "))))


;; org-babel
(org-babel-do-load-languages    ;;; org7.5/doc/org.pdf p162
  'org-babel-load-languages
  '((R . t)
    (sh . t)
    (C . t)
    (dot . t)))

;; org ファイル読み込み時に自動的に画像をインライン表示する
;; 読み込み後、編集中の画像リンクには影響しない
(setq org-startup-with-inline-images t)


;; 常に画像を表示
;; リンク記述後 C-l で即表示
(add-hook 'org-mode-hook 'turn-on-iimage-mode)

;;;;;;;;;;;;;;;; mobile-orgとの同期 START ;;;;;;;;;;;;;;;;;;;;
;;;;; 参考 http://tokikane-tec.blogspot.jp/2015/01/org-mobile-pullpush_21.html
(require 'org-mobile)
;; (defvar org-mobile-sync-timer nil
;;   "Timer that `org-mobile-sync-timer' used to reschedule itself, or nil.")

;; ;;pullしてpush する関数
;; (defun org-mobile-sync nil
;;   (interactive)
;;   (org-mobile-pull)
;;   (org-mobile-push))

;; ;;idle 時間が30秒経過すると自動でpullしてpush
;; (defun org-mobile-set-sync-timer nil
;;   (interactive)
;;   (setq org-mobile-sync-timer (run-with-idle-timer 10 30 t 'org-mobile-sync)))

;; ;;自動pull, push  を無効にする
;; (defun org-mobile-clear-sync-timer nil
;;   (interactive)
;;   (cancel-timer org-mobile-sync-timer))

;; ;;指定されたファイルを指定された時間で監視，更新されたらorg-mobile-pull を実行
;; (defun install-monitor (file secs)
;;   (run-with-timer
;;    0 secs
;;    (lambda (f p)
;;      (unless (< p (second (time-since (elt (file-attributes f) 5))))
;;        (org-mobile-pull)
;;        ;; (message "mobileorg.org is updated!")
;;        ))
;;    file secs))

;; ;; capture ファイルを10秒ごとに監視，更新されたらorg-mobile-pull 実行
;; (install-monitor (file-truename
;;                   (concat
;;                    (file-name-as-directory org-mobile-directory)
;;                    org-mobile-capture-file))
;;                  10)

;; ;; org-mobile に登録したファイルを保存するとpush
;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (when (eq major-mode 'org-mode)
;;               (dolist (file (org-mobile-files-alist))
;;                 (if (string= (file-truename (expand-file-name (car file)))
;;                              (file-truename (buffer-file-name)))
;;                     (org-mobile-push)))
;;               )))

;; ;; org-mobile に登録したファイルを開くとpull
;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (when (eq major-mode 'org-mode)
;;               (dolist (file (org-mobile-files-alist))
;;                 (if (string= (file-truename (expand-file-name (car file)))
;;                              (file-truename (buffer-file-name)))
;;                     (org-mobile-pull)))
;;               )))

;; (org-mobile-set-sync-timer)
;;;;;;;;;;;;;;;;;;; mobile-orgとの同期 END ;;;;;;;;;;;;;;;;;;;;;;;

;; どうもこれが有効になってるとwindows 10 で時折固まるので、コメントアウトする 2017/5/19(金)
;; (require 'org-mobile-sync)
;; (org-mobile-sync-mode 1)


(require 'ox-freemind)

;; anything初期設定
(require 'anything-startup)
;; 15.5 anything-for-files
(global-set-key (kbd "C-x C-.") 'anything-for-files)
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)
;; (auto-install-from-url "http://www.emacswiki.org/cgi-bin/emacs/download/descbinds-anything.el")
(when (require 'descbinds-anything nil t)
  ;; describe-bindings を Anything に置き換える
  (descbinds-anything-install))

;; helm
(require 'helm-config)
(helm-mode 1)

;; (require 'helm-config)
;; (helm-descbinds-mode)
;; (require 'helm-migemo)
;; (setq helm-use-migemo t)
;; (define-key global-map (kbd "C-.") 'helm-for-files)
;; (define-key global-map (kbd "C-x b") 'helm-for-files)
;; (define-key global-map (kbd "M-y") 'helm-show-kill-ring)

;; 2.1 ddskk
(defun skk-latin-toggle()
  (interactive)
  (if skk-mode
      (if skk-latin-mode
          (skk-mode t)
        (progn
          (skk-latin-mode t)
          (key-combo-mode t)))))

(global-set-key "\C-\\" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xt" 'skk-tutorial)
(global-set-key (kbd "C-t") 'skk-latin-toggle)
(autoload 'skk-mode "skk" nil t)
(autoload 'skk-tutorial "skk-tut" nil t)
(autoload 'skk-check-jisyo "skk-tools" nil t)
(autoload 'skk-merge "skk-tools" nil t)
(autoload 'skk-diff "skk-tools" nil t)
;;;;  "「"を入力したら"」"も自動で挿入
(setq skk-auto-insert-paren t)
;;;;  句読点は , . を使う
(setq skk-kuten-touten-alist
  '(
    (jp . ("。" . "、" ))
    (en . ("．" . "，"))
    ))
;;;;  jp にすると「。、」を使います
(setq-default skk-kutouten-type 'jp)
;;;;  @で挿入する日付表示を半角に
(setq skk-number-style nil)
;;;;  変換のときEnterを押しても確定のみで改行しない。
(setq skk-egg-like-newline t)

;; skk で日本語入力時に \ 押下で skk-list-chars が起動しないようにする
(defun skk-list-chars (&optional arg)
  (interactive "P")
  ;; skk-list-chars が呼ばれた時点で元の▽モードが終了してしまうので、再度▽モードを呼び出す
  (skk-set-henkan-point-subr)
  )
(add-hook 'skk-mode-hook
          (lambda ()
            (key-combo-mode -1)))

;; 2.2 auto-install.el
(if (linux?)
    (progn
      (require 'auto-install)
      (auto-install-update-emacswiki-package-name t)
      (auto-install-compatibility-setup)
      (setq auto-install-use-wget t)
      (auto-install-compatibility-setup)
      (setq auto-install-directory "~/Dropbox/.emacs.d/elisp/")
      (setq ediff-window-setup-function 'ediff-setup-windows-plain)))

;; 3.9 key-chord.el
;; (require 'key-chord)
;; (setq key-chord-two-keys-delay 0.04)
;; (key-chord-mode 1)

;; 4.2 uniquify.el
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniqify-ignore-buffers-re "*[^*]+*")

;; 4.4 recentf.el
;; (setq recentf-max-saved-items 500)
;; (setq recentf-exclude '("/TAGS$" "/var/tmp/"))
(require 'recentf-ext)
;; (define-key global-map (kbd "C-x f") 'recentf-open-files)

;; 4.6 emacsclient
;; (server-start)
;; (defun iconify-emacs-when-server-is-done ()
;;   (unless server-clients (iconify-frame)))
;; ;; 編集が終了したらEmacsをアイコン化する
;; (add-hook 'server-done-hook 'iconify-emacs-when-server-is-done)
;; ;;
;; (global-set-key (kbd "C-x C-c") 'server-edit)
;; (defalias 'exit 'save-buffers-kill-emacs)

;; 4.8 auto-save-buffers.el
;; ファイルを自動保存する
;; M-x install-elisp http://homepage3.nifty.com/oatu/emacs/archives/auto-save-buffers.el
(require ' auto-save-buffers)
(run-with-idle-timer 2 t 'auto-save-buffers)

(when (executable-find "cmigemo")
  ;; 5.5 migemo.el
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\g"))
  ;; migemo-dictのパスを指定
  (if (linux?)
      (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
    (setq migemo-dictionary (expand-file-name "~/Dropbox/.emacs.d/elisp/migemo/cp932/migemo-dict")))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  ;; キャッシュ機能を利用する
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)
  ;; 辞書の文字コードを指定．
  (if (linux?)
      (setq migemo-coding-system 'utf-8-unix)
    (setq migemo-coding-system 'cp932-unix))
  (load-library "migemo")
  (migemo-init)
)

(when (not (executable-find "cmigemo"))
  (setq migemo-isearch-enable-p nil))

;; 5.6 point-undo.el
;; カーソル位置を戻す
;; (require 'point-undo)
;; (define-key global-map (kbd "C--") 'point-undo)
;; (define-key global-map (kbd "C-=") 'point-redo)

;; 5.8 goto-chg.el
;; 最後の変更箇所にジャンプする
(require 'goto-chg)
(define-key global-map (kbd "<f8>") 'goto-last-change)
(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)

;; ;; 6.2 redo+.el
;; (require 'redo+)
;; (global-set-key (kbd "C-M-/") 'redo)
;; (setq undo-no-redo t)
;; (setq undo-limit 600000)
;; (setq undo-strong-limit 900000)

(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (global-set-key (kbd "C-M-/") 'undo-tree-redo))


;; 6.6 yasnippet.el
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory "~/Dropbox/.emacs.d/elisp/plugins/yasnippet-0.6.1c/snippets")

;; 6.14 auto-complete.el
;; M-x auto-install-batch auto-complete TAB
(require 'auto-complete-config)
(global-auto-complete-mode 1)
;; C-n/C-p で候補を選択
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
;; 大文字、小文字を区別する
(setq ac-ignore-case nil)
;; auto-complete の候補に日本語を含む単語が含まれないようにする
;; http://d.hatena.ne.jp/IMAKADO/20090813/1250130343
(defadvice ac-word-candidates (after remove-word-contain-japanese activate)
  (let ((contain-japanese (lambda (s) (string-match (rx (category japanese)) s))))
    (setq ad-return-value
          (remove-if contain-japanese ad-return-value))))

;; 7.6 color-moccur.el
(require 'color-moccur)
(setq moccur-split-word 1) ; スペースで区切られた複数の単語にマッチさせる
(setq moccur-use-migemo 1)

;; 7.7 moccur-edit.el
(require 'moccur-edit)

(when (not (executable-find "grep"))
  ;; 7.9 igrep.el
  (require 'igrep)
  ;; lgrepに-0u8オプションをつけると出力がUTF-8になる
  (igrep-define lgrep (igrep-use-zgrep nil) (igrep-regex-option "-n -0u8"))
  (igrep-find-define lgrep (igrep-use-zgrep nil) (igrep-regex-option "-n -0u8"))

  ;; 7.10 grep-a-lot.el
  (require 'grep-a-lot)
  (grep-a-lot-setup-keys)
  ;; igrepを使う人向け
  (grep-a-lot-advise igrep)

  ;; grep-a-lot-buffer-name の定義を上書き
  ;; 参考 http://d.hatena.ne.jp/kitokitoki/20110213/p1
  (setq my-grep-a-lot-search-word nil)
  (defun grep-a-lot-buffer-name (position)
    "Return name of grep-a-lot buffer at POSITION."
    (if (not (null my-grep-a-lot-search-word))
        (concat "*grep*<" my-grep-a-lot-search-word ">")
      (concat "*grep*<" (number-to-string position) ">")))

  (defadvice rgrep (before my-rgrep (regexp &optional files dir) activate)
    (setq my-grep-a-lot-search-word regexp))

  (defadvice lgrep (before my-lgrep (regexp &optional files dir) activate)
    (setq my-grep-a-lot-search-word regexp))


  (defadvice grep (before my-lgrep (regexp &optional files dir) activate)
    (if (string-match "|.+'\\(.+\\)'" regexp)
        ;; 検索ワードが '(シングルクォート)に囲まれていることを期待
        (setq my-grep-a-lot-search-word
              (subseq regexp (match-beginning 1) (match-end 1)))))

  ;; 7.11 grep-edit.el
  (require 'grep-edit)
)
;; 8.4 w3m.el
;; (if (linux?)
;;    (require 'w3m-load))

;; ;; 8.5 twittering-mode.el
(require 'twittering-mode)
(global-set-key (kbd "C-x t") 'twit)
(setq twittering-status-format
      "%C{%Y/%m/%d %H:%M:%S} %s > %T // from %f%L%r%R")
(setq twittering-auth-method 'xauth)
(setq twittering-username "sakugoe_hituji")
(if (win?)
  ;; windows の twittering-mode の認証が失敗しないための対策
  (setq twittering-allow-insecure-server-cert t))
;; F お気に入り
;; R 公式リツイート
;; Q 引用リツイート QT
(define-key twittering-mode-map (kbd "F") 'twittering-favorite)
(define-key twittering-mode-map (kbd "R") 'twittering-native-retweet)
(define-key twittering-mode-map (kbd "Q") 'twittering-organic-retweet)

;; 8.6 gist.el
;(require 'gist)

;; 11.1 view-mode
;; view-minor-modeの設定
(setq view-read-only t)
(add-hook 'view-mode-hook
          '(lambda()
             (progn
               ;; C-b, ←
               (define-key view-mode-map "h" 'backward-char)
               ;; C-n, ↓
               (define-key view-mode-map "j" 'next-line)
               ;; C-p, ↑
               (define-key view-mode-map "k" 'previous-line)
               ;; C-f, →
               (define-key view-mode-map "l" 'forward-char)
               )))

;; 12.3 paredit.el
;(require 'paredit)
;(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;(add-hook 'lisp-interaction-mode 'enable-paredit-mode)
;(add-hook 'lisp-mode-hook 'enable-paredit-mode)
;(add-hook 'ielm-mode-hook 'enable-paredit-mode)

;; kiwanamiさん作成関連
;; (autoload 'id-manager "id-manager" nil t)
;; (global-set-key (kbd "M-7") 'id-manager)                     ; キーバインド
;; (setenv "GPG_AGENT_INFO" nil)                                ; minibufferでパスワードを入力する場合

;;;;;;;;;;;;;;;;;  VCS関連 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (install-elisp "https://raw.github.com/byplayer/egg/master/egg.el")
;; (install-elisp "https://raw.github.com/byplayer/egg/master/egg-grep.el")
(when (executable-find "git")
  (require 'egg nil t))

;; (install-elisp "http://www.xsteve.at/prg/emacs/psvn.el")
;; 上記ファイルでは svn 1.7 で使えなかった(2012/6/22時点)
;; (install-elisp "http://www.eaflux.com/psvn/psvn.el.new")
(when (executable-find "svn")
  (setq svn-status-verbose nil)
  (autoload 'svn-status "psvn" "Run 'svn status'." t))
(when (win?)
  (setq process-coding-system-alist '(("svn" . utf-8)))
  (setq default-file-name-coding-system 'sjis)
  (setq svn-status-svn-file-coding-system 'utf-8)
  (setq svn-status-svn-process-coding-system 'utf-8)
  (setenv "CYGWIN" "nodosfilewarning")
  (setenv "LC_ALL" "en_US.UTF-8")
  (setenv "LANG" "en_US.UTF-8")
  )

;; 2017/1/29(日) windows10 で clojureを開発するにあたり、inf-clojureを導入した
;; inf-clojure で参考にしたページでgtagsを使っていたので、
;; gtagsを使うことにする。よってctags部分をコメント化する
;;;;;;;;;;;; START ;;;;;;;;;;;;;;;
;;; ctags.el の設定(Emacs 実践入門 p191より)
;; (require 'ctags nil t)
;; (setq tags-revert-without-query t)
;; ;; (setq ctags-command "ctags -e -R ")
;; ;; ctagsを呼び出すコマンドライン
;; (setq ctags-command "ctags -R --fields=\"+afikKlmnsSzt\" ")
;; (global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)
;; ;; 定義ジャンプできるようにする
;; (when (require 'anything nil t)
;;   (require 'anything-exuberant-ctags)
;;   )
;; ;; 関数の定義にジャンプ
;; (global-set-key (kbd "C-;") (lambda () (interactive)
;;                               (ring-insert find-tag-marker-ring (point-marker))
;;                               (anything-exuberant-ctags-select-from-here)))
;; ;; 関数の一覧を表示
;; (global-set-key (kbd "C-o") (lambda () (interactive)
;;                               (ring-insert find-tag-marker-ring (point-marker))
;;                               (anything-exuberant-ctags-select)))
;; ;; ジャンプ元に戻る
;; (global-set-key (kbd "C--") 'pop-tag-mark)
;;;;;;;;;;;; END ;;;;;;;;;;;;;;;

;;; Enable helm-gtags-mode
(require 'helm-gtags)
(setq helm-gtags-auto-update t)
(setq helm-gtags-update-interval-second 20)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'clojure-mode-hook 'helm-gtags-mode)
(add-hook 'emacs-lisp-mode-hook 'helm-gtags-mode)
(add-hook 'lisp-mode-hook 'helm-gtags-mode)

;; customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(package-selected-packages
   (quote
    (init-loader org-mobile-sync alda-mode slime macrostep auto-complete neotree elscreen-persist elscreen-buffer-group package-utils persp-mode window-layout helm-ag cdb ccc ddskk helm-core popup async helm helm-projectile helm-gtags gtags inf-clojure ripgrep todotxt-mode ruby-block quickrun melpa key-combo helm-migemo helm-descbinds flymake flycheck emmet-mode elscreen ctags clojure-cheatsheet ac-nrepl))))

;; clojureでタグ名にネームスペースがつかないようにする
;; http://ayato.hateblo.jp/entry/20150607/1433653213
;;;;;;;;;;;; START ;;;;;;;;;;;;;;;
(add-to-list 'helm-gtags--prompt-alist '(tag-without-ns . "Find Definition: "))

(defun my/helm-gtags--read-tagname (type &optional default-tagname)
  (let ((tagname (helm-gtags--token-at-point type))
        (prompt (assoc-default type helm-gtags--prompt-alist))
        (comp-func (assoc-default type helm-gtags-comp-func-alist)))
    (if (and tagname helm-gtags-use-input-at-cursor)
        tagname
      (when (and (not tagname) default-tagname)
        (setq tagname default-tagname))
      (when (eq type 'tag-without-ns)
        (setq tagname (first (last (split-string tagname "/")))))
      (when tagname
        (setq prompt (format "%s(default \"%s\") " prompt tagname)))
      (let ((completion-ignore-case helm-gtags-ignore-case)
            (completing-read-function 'completing-read-default))
        (completing-read prompt comp-func nil nil nil
                         'helm-gtags--completing-history tagname)))))

(defun my/find-tag-without-ns (tag)
  (interactive
   (list (my/helm-gtags--read-tagname 'tag-without-ns)))
  (helm-gtags--common '(helm-source-gtags-tags) tag))

(defun my/helm-gtags--find-tag-from-here-init ()
  (helm-gtags--find-tag-directory)
  (helm-gtags--save-current-context)
  (let ((token (helm-gtags--token-at-point 'from-here)))
    (unless token
      (error "Cursor is not on symbol."))
    (let* ((filename (helm-gtags--real-file-name))
           (from-here-opt (format "--from-here=%d:%s"
                                  (line-number-at-pos)
                                  (if (helm-gtags--convert-cygwin-windows-file-name-p)
                                      (cygwin-convert-file-name-to-windows filename)
                                    filename))))
      (setq token (first (last (split-string token "/"))))
      (setq helm-gtags--last-input token)
      (with-current-buffer (helm-candidate-buffer 'global)
        (let* ((default-directory (helm-gtags--base-directory))
               (status (process-file "global" nil '(t nil) nil
                                     "--result=grep" from-here-opt token)))
          (helm-gtags--remove-carrige-returns)
          (unless (zerop status)
            (cond ((= status 1)
                   (error "Error: %s%s" (buffer-string) filename))
                  ((= status 3)
                   (error "Error: %s" (buffer-string)))
                  (t (error "%s: not found" token)))))))))

(defvar my/helm-source-gtags-find-tag-from-here
  (helm-build-in-buffer-source "Find tag from here"
    :init 'my/helm-gtags--find-tag-from-here-init
    :candidate-number-limit helm-gtags-maximum-candidates
    :real-to-display 'helm-gtags--candidate-transformer
    :persistent-action 'helm-gtags--persistent-action
    :fuzzy-match helm-gtags-fuzzy-match
    :action helm-gtags--find-file-action))

(defun my/helm-gtags-find-tag-from-here ()
  "Jump point by current point information.
Jump to definition point if cursor is on its reference.
Jump to reference point if curosr is on its definition"
  (interactive)
  (helm-gtags--common '(my/helm-source-gtags-find-tag-from-here) nil))

;;;;;;;;;;;; END ;;;;;;;;;;;;;;;

;; key bindings
(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "C-M-;") 'my/find-tag-without-ns)
  (define-key helm-gtags-mode-map (kbd "C-;") 'my/helm-gtags-find-tag-from-here)
  (define-key helm-gtags-mode-map (kbd "C-M-r") 'helm-gtags-find-rtag)
  (define-key helm-gtags-mode-map (kbd "C-M-s") 'helm-gtags-find-symbol)
  ;; (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "C--") 'helm-gtags-pop-stack))

(require 'helm-ag)
; agのデフォルトのコマンドオプションを指定
; -nを消すとサブディレクトリも再帰的に検索します
(setq helm-ag-base-command "ag --nocolor --nogroup")
(define-key global-map (kbd "M-O") 'helm-ag)
(define-key global-map (kbd "M-o") 'helm-ag-this-file)

;;;;;;;;;;;;;;;;; doxygen START ;;;;;;;;;;;;;;;;;;;;;;;;;
;; doxygenによるコールグラフの生成
(defun create-doxygen-and-open()
  (interactive)
  ;; TODO 拡張子に応じて使用するテンプレートを切り分ける
  ;; もしくはそのときのモードに応じて呼び出す関数を分ける
  (async-shell-command "~/WORK/bin/doxygenopen.sh"))

(global-set-key (kbd "<f9>") 'create-doxygen-and-open)
;;;;;;;;;;;;;;;;; doxygen END   ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  言語モード ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; プログラミング全般
;; 参考
;; http://moimoitei.blogspot.jp/2010/05/flymake-in-emacs.html
(require 'flymake)

;; GUIの警告は表示しない
(setq flymake-gui-warnings-enabled nil)

;; 全てのファイルで flymakeを有効化
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; M-p/M-n で警告/エラー行の移動
(global-set-key "\M-p" 'flymake-goto-prev-error)
(global-set-key "\M-n" 'flymake-goto-next-error)

;; 警告エラー行の表示
(global-set-key "\C-cd" 'my-flymake-display-err-popup.el-for-current-line)

;; popup.el を使って tip として表示
(defun my-flymake-display-err-popup.el-for-current-line ()
  "Display a menu with errors/warnings for current line if it has errors and/or warnings."
  (interactive)
  (let* ((line-no            (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (menu-data          (flymake-make-err-menu-data line-no line-err-info-list)))
    (if menu-data
      (popup-tip (mapconcat '(lambda (e) (nth 0 e))
                            (nth 1 menu-data)
                            "\n")))
    ))

;; 参考
;; http://d.hatena.ne.jp/CortYuming/20110920/p1
(when (load "flymake" t)

  ;; JavaScript with Google Closure
  ;; http://www.emacswiki.org/emacs/FlymakeJavaScript
  ;; http://code.google.com/intl/ja/closure/utilities/docs/linter_howto.html
  ;; http://d.hatena.ne.jp/Ehren/20101006/1286386194
  ;; http://d.hatena.ne.jp/Ehren/20110912/1315804158
  (defun flymake-gjslint-init ()
    "Initialize flymake for gjslint"
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace)))
      (list "/usr/local/bin/gjslint" (list temp-file "--nosummary"))))

  (add-to-list 'flymake-allowed-file-name-masks
               '(".+\\.js$"
                 flymake-gjslint-init
                 flymake-simple-cleanup
                 flymake-get-real-file-name))
  (add-to-list 'flymake-err-line-patterns
               '("^Line \\([[:digit:]]+\\), E:[[:digit:]]+: "
                 nil 1 nil))
  (add-hook 'js-mode-hook (lambda () (flymake-mode t)))


  ;; FlymakeHtml
  ;; http://www.emacswiki.org/emacs/FlymakeHtml
  (delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
  (defun flymake-html-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      ;; (list "tidy" (list local-file))))
      (list "tidy" (list "-utf8" local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.html$\\|\\.ctp" flymake-html-init))
  (add-to-list 'flymake-err-line-patterns
               '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
                 nil 1 2 4))
  (add-hook 'html-mode-hook '(lambda () (flymake-mode t)))
  (add-hook 'nxml-mode-hook '(lambda () (flymake-mode t)))


  ;;;;;;;;;;;;;;;;;;
  ;; error avoidance
  ;; http://d.hatena.ne.jp/sugyan/20100705/1278306885
  (defadvice flymake-post-syntax-check
    (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  )

;; ;; flycheck
;; (setq flycheck-check-syntax-automatically '(mode-enabled save))
;; (add-hook 'js-mode-hook 'flycheck-mode)
;; (add-hook 'html-mode-hook 'flycheck-mode)
;; (add-hook 'php-mode-hook 'flycheck-mode)
;; (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

;; html や css の編集に便利な emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(setq emmet-move-cursor-between-quotes t) ;; default nil

;; emmet-mode を入れたので zencoding-mode はコメントアウトする
;; ;; Zen Coding Mode
;; (require 'zencoding-mode)
;; (add-hook 'sgml-mode-hook 'zencoding-mode)
;; (define-key zencoding-mode-keymap (kbd "C-z") 'zencoding-expand-yas)
;; (define-key zencoding-mode-keymap (kbd "<C-return>") nil)
;; (define-key zencoding-mode-keymap (kbd "<S-return>") 'zencoding-expand-line)


;;; コードの折り畳み
;; C coding style
(add-hook 'c-mode-hook
          '(lambda ()
    (hs-minor-mode 1)
    ;; 2015/2/14 なぜか k が prefix command になってしまい
    ;; 入力できなくなったので美しくないが以下を実行
    (global-set-key (kbd "k") 'self-insert-command)
    ))
;; Scheme coding style
(add-hook 'scheme-mode-hook
          '(lambda ()
    (hs-minor-mode 1)))
;; Elisp coding style
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
    (hs-minor-mode 1)))
;; Lisp coding style
(add-hook 'lisp-mode-hook
          '(lambda ()
    (hs-minor-mode 1)))
;; Python coding style
(add-hook 'python-mode-hook
          '(lambda ()
    (hs-minor-mode 1)))
;; javascript coding style
(add-hook 'js-mode-hook
          '(lambda ()
    (hs-minor-mode 1)))


(define-key global-map (kbd "C-#") 'hs-toggle-hiding)
(define-key global-map (kbd "C-8") 'hs-hide-block)
(define-key global-map (kbd "C-9") 'hs-show-block)
(define-key global-map (kbd "C-M-8") 'hs-hide-all)
(define-key global-map (kbd "C-M-9") 'hs-show-all)

;;;;;; JavaScript関連の設定
;; moz.el
(add-hook 'js-mode-hook
          (lambda ()
            (moz-minor-mode 1) ;; 要moz.el
            (local-set-key "\C-c\C-j" 'run-js-console-and-split-window)
            (local-set-key "\C-cr" 'js-console-execute-region)
            ))

;;; ejacs
;; C-c C-jでjs-consoleを起動
;; C-c rで選択範囲を実行
(autoload 'js-console "js-console" nil t)
(defun js-console-execute-region (start end)
  "Execute region"
  (interactive "r")
  (let ((buf-name (buffer-name (current-buffer))))
    (copy-region-as-kill start end)
    (switch-to-buffer-other-window "*js*")
    (js-console-exec-input (car kill-ring))
    (switch-to-buffer-other-window buf-name)))
(defun run-js-console-and-split-window ()
  "Run js-console and split window horizontally."
  (interactive)
  (split-window-horizontally)
  (js-console)
  (other-window 1)
  )
(add-hook 'js-mode-hook
          (lambda ()
            (moz-minor-mode 1) ;; 要moz.el
            (local-set-key "\C-c\C-j" 'run-js-console-and-split-window)
            (local-set-key "\C-cr" 'js-console-execute-region)
            (key-combo-define-local (kbd "{ RET") "{\n`!!'\n}")
            ))

;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

;;     (add-hook 'javascript-mode-hook 'javascript-custom-setup)
;;     (defun javascript-custom-setup ()
;;       (moz-minor-mode 1))

;;;;;; PHPに関する設定
;; php-mode
(load-library "php-mode")
(require 'php-mode)
(add-hook 'php-mode-user-hook
	  '(lambda ()
	     (setq tab-width 2)
	     (setq indent-tabs-mode nil)))
;(setq php-mode-force-pear t) ;PEAR規約のインデント設定にする

;; php-completion
(add-hook 'php-mode-hook
         (lambda ()
             (require 'php-completion)
             (php-completion-mode t)
             (define-key php-mode-map (kbd "\C-co") 'phpcmp-complete)
             (when (require 'auto-complete nil t)
             (make-variable-buffer-local 'ac-sources)
             (add-to-list 'ac-sources 'ac-source-php-completion)
             (auto-complete-mode t))))

;;;;;; Ruby関連の設定
;;; 参考
;;  1) http://futurismo.biz/archives/2213
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; (require 'ruby-electric)
;; (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
;; (setq ruby-electric-expand-delimiters-list nil)

;; ruby-block.el --- highlight matching block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; TODO Rubyでタグジャンプ

;;;;;; Lisp関連の設定
;; slime
(when (require 'slime nil t)
  (slime-setup '(slime-repl slime-fancy slime-banner))

  ;; sbclのパスを記述
  (if (linux?)
      (progn
    (setq inferior-lisp-program "sbcl")
    ;; HyperSpecを読み込む.
    ;; HyperSpecがインストールされている場所「/usr/share/doc/hyperspec/」
    (setq common-lisp-hyperspec-root
          (concat "file://" (expand-file-name "~/Dropbox/.emacs.d/doc/HyperSpec/")))
    (setq common-lisp-hyperspec-symbol-table
          (expand-file-name "~/Dropbox/.emacs.d/doc/HyperSpec/Data/Map_Sym.txt"))

    ;; HyperSpecをw3mで見る
    (defadvice common-lisp-hyperspec
      (around hyperspec-lookup-w3m () activate)
      (let* ((window-configuration (current-window-configuration))
         (browse-url-browser-function
          `(lambda (url new-window)
             (w3m-browse-url url nil)
             (let ((hs-map (copy-keymap w3m-mode-map)))
               (define-key hs-map (kbd "q")
             (lambda ()
               (interactive)
               (kill-buffer nil)
               (set-window-configuration ,window-configuration)))
               (use-local-map hs-map)))))
        ad-do-it)))
    ;; windows の場合
    (setq inferior-lisp-program "sbcl.exe")))

;; 日本語利用
(setq slime-net-coding-system 'utf-8-unix)

(add-hook 'lisp-mode-hook (lambda ()
                            (slime-mode t)
                            (show-paren-mode t)))

;; C-c L で slimeを起動
(defun my-slime (&optional command coding-system)
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*lisp*"))
  (slime command coding-system))
(global-set-key "\C-cL" 'my-slime)
(global-set-key "\C-cs" 'slime-selector)

;;; Modern Common Lisp より
;;; http://modern-cl.blogspot.com/
;; http://d.hatena.ne.jp/m2ym/20110120/1295524932
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
;; Apropos
(push '("*slime-apropos*") popwin:special-display-config)
;; Macroexpand
(push '("*slime-macroexpansion*") popwin:special-display-config)
;; Help
(push '("*slime-description*") popwin:special-display-config)
;; Compilation
(push '("*slime-compilation*" :noselect t) popwin:special-display-config)
;; Cross-reference
(push '("*slime-xref*") popwin:special-display-config)
;; Debugger
(push '(sldb-mode :stick t) popwin:special-display-config)
;; REPL
(push '(slime-repl-mode) popwin:special-display-config)
;; Connections
(push '(slime-connection-list-mode) popwin:special-display-config)

;; ウィンドウ間移動のキーバインドが上書きされないようにする
(add-hook
 'slime-repl-mode-hook
 (lambda ()
   (local-set-key (kbd "<C-up>") (quote windmove-up))
   (local-set-key (kbd "<C-down>") (quote windmove-down))))

(eval-after-load "slime-repl"
  '(progn
     (define-key slime-repl-mode-map (kbd "<C-up>") nil)
     (define-key slime-repl-mode-map (kbd "<C-down>") nil)))

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; 正しいインデントにする
(when (require 'cl-indent-patches nil t)
  ;; emacs-lispのインデントと混同しないように
  (setq lisp-indent-function
        (lambda (&rest args)
          (apply (if (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
                     'lisp-indent-function
                     'common-lisp-indent-function)
                 args))))

;; scheme 用の設定
(if (win?)
    (progn (setq process-coding-system-alist
                 (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
           (setq scheme-program-name "gosh -i")
           (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
           (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t))
  (setq scheme-program-name "gosh"))

(require 'cmuscheme)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cS" 'scheme-other-window)

;; カーソルのあるポイントのinfoを引く
(defvar anything-c-source-info-gauche-refj
   ;; '((info-index . "~/../gauche/share/info/gauche-refj.info")))
  '((info-index . "gauche-refj.info")))
(defun anything-info-ja-at-point ()
  "Preconfigured `anything' for searching info at point."
  (interactive)
  (anything '(anything-c-source-info-gauche-refj)
            (thing-at-point 'symbol) nil nil nil "*anything info*"))
(define-key global-map (kbd "C-M-i") 'anything-info-ja-at-point)

;;;;;; Java
(add-hook 'java-mode-hook
      '(lambda ()
         (key-combo-define-local (kbd "(") '("(`!!')"))
         (key-combo-define-local (kbd "()") "()")
         (key-combo-define-local (kbd "((") "((`!!'))")
         (key-combo-define-local (kbd "(\"") "(\"`!!'\")")
         (key-combo-define-local (kbd "\"") '("\"`!!'\""))
         (key-combo-define-local (kbd "\"\"") "\"\"")
         (key-combo-define-local (kbd "{") '("{`!!'}"))
         (key-combo-define-local (kbd "{}") "{}")
         (key-combo-define-local (kbd "[") '("[`!!']"))
         (key-combo-define-local (kbd "[]") "[]")
         ))

;;;;;; clojure
(require 'clojure-mode)
;; (autoload 'inf-clojure "inf-clojure" "Run an inferior Clojure process" t)
;; (add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)
;; (setq inf-clojure-prompt-read-only nil)

;;; ciderは編集が重くなったり、replの起動に1分近くかかる問題から、一旦無効にする 2017/1/29(日)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; START ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)))

;; ウィンドウ間移動のキーバインドが上書きされないようにする
(add-hook
 'cider-repl-mode-hook
 (lambda ()
   (local-set-key (kbd "<C-up>") (quote windmove-up))
   (local-set-key (kbd "<C-down>") (quote windmove-down))))

;; ciderのreplでauto-completeが使えるようにする
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

;; ciderのスタックトレースが明るくて見にくいため修正
(setq cider-stacktrace-frames-background-color "#003200320032")
;;;;;;;;;;;;;;;;;;;;;;;;;;;  END  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sql-mode
(add-hook 'sql-mode-hook
          (lambda () (auto-complete-mode t)))

;; graphviz-dot-mode
(add-hook 'graphviz-dot-mode-hook
          (lambda () (auto-complete-mode t)))

;; plantuml-mode
(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
(autoload 'plantuml-mode "plantuml-mode" "PlantUML mode" t)

(add-hook 'plantuml-mode-hook
          (lambda () (auto-complete-mode t)))

(defun plantuml-execute ()
  (interactive)
  (when (buffer-modified-p)
    (map-y-or-n-p "Save this buffer before executing PlantUML?"
                  'save-buffer (list (current-buffer))))
  (let ((code (buffer-string))
        out-file
        cmd)
    (when (string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s*$" code)
      (setq out-file (match-string 1 code)))
    (setq cmd (concat
               "java -jar " plantuml-java-options " "
               (shell-quote-argument plantuml-jar-path) " "
               (and out-file (concat "-t" (file-name-extension out-file))) " "
               plantuml-options " "
               (buffer-file-name)))
    (message cmd)
    (shell-command cmd)
    (message "done")))

(if (linux?)
    (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (setq plantuml-jar-path "C:/opt/jars/plantuml.jar")
  )
(setq plantuml-java-options "")
(setq plantuml-options "-charset UTF-8")
(setq plantuml-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c") 'plantuml-execute)
        map))

;;;; 単語補完
(require 'pabbrev)

;; ElScreen
(when (require 'elscreen nil t)
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs))
  (define-key global-map [\C-\S-\iso-\lefttab] 'elscreen-previous)
  (define-key global-map [\C-\S-\tab] 'elscreen-previous)
  (define-key global-map [\C-\tab] 'elscreen-next)
  (define-key global-map (kbd "C-S-t") 'elscreen-create)
  (elscreen-start)
  ;; (elscreen-separate-buffer-list-mode 1)
  ;; (elscreen-persist-mode 1)
  )
(put 'upcase-region 'disabled nil)

;;;; alda-mode 2017/5/4(木)
(require 'alda-mode)
(when (win?)
  (setq alda-binary-location "C:/opt/alda/1.0.0/bin/alda.exe"))



;;; windmove (e2wm でバッファ切り換えがしやすくなるように)
(windmove-default-keybindings)
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-left>"))

(global-set-key (kbd "<C-up>") (quote windmove-up))
(global-set-key (kbd "<C-down>") (quote windmove-down))
(global-set-key (kbd "<C-right>") (quote windmove-right))
(global-set-key (kbd "<C-left>") (quote windmove-left))

;; ;; persp-modeを導入 2017/2/11(土)
;; (setq persp-keymap-prefix (kbd "C-c p")) ;prefix
;; (setq persp-add-on-switch-or-display t) ;バッファを切り替えたら見えるようにする
;; (persp-mode 1)
;; (defun persp-register-buffers-on-create ()
;;   (interactive)
;;   (dolist (bufname (condition-case _
;;                        (helm-comp-read
;;                         "Buffers: "
;;                         (mapcar 'buffer-name (buffer-list))
;;                         :must-match t
;;                         :marked-candidates t)
;;                      (quit nil)))
;;     (persp-add-buffer (get-buffer bufname))))
;; (add-hook 'persp-activated-hook 'persp-register-buffers-on-create)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
