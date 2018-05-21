;; melpa
(require 'package)

; Add package-archives
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

; Initialize
(package-initialize)

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
  (add-to-list 'load-path "C:/opt/emacs/site-lisp/emu"))

;; http://pastelwill.jp/wiki/doku.php?id=emacs:org-mode
;; org-mode の開発版、安定板を切り替える
(setq load-path (append '(
			  "~/Dropbox/.emacs.d/public_repos/org-mode/head/lisp"       ; The latest org-mode
              "~/Dropbox/.emacs.d/public_repos/org-mode/head/contrib/lisp"
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
  (global-set-key (kbd "C-x C-a") 'set-frame-parameter-alpha)

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

(setq-default tab-width 4 indent-tabs-mode nil)

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

;; C-oで次のwindowへカーソルを移す
(global-set-key (kbd "C-o") 'other-window-or-split)

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
(global-set-key (kbd "C-M-g") 'igrep)
(global-set-key (kbd "C-x g") 'grep-find)
(global-set-key (kbd "C-M-e") 'ediff-merge-files)
(global-set-key "\M-o" 'occur-by-moccur)
(global-set-key (kbd "C-M-o") 'moccur)
(global-set-key (kbd "C-x C-o") 'moccur-grep-find)
;; (global-set-key [\C-\tab] 'dabbrev-expand)
(global-set-key (kbd "C-c C-i") 'indent-region)
(global-set-key (kbd "C->") 'comment-region)
(global-set-key (kbd "C-<") 'uncomment-region)
;; 改行と同時にインデント
(global-set-key (kbd "C-m") 'newline-and-indent)
;; (global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "C-c h") 'help-command)
;; 文字の拡大、縮小、元に戻す
(global-set-key (kbd "C-M-;") (lambda () (interactive) (text-scale-increase 1)))
;; 文字の縮小
(global-set-key (kbd "C-M--") (lambda () (interactive) (text-scale-decrease 1)))
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

;;;;;;;;;;;;;;;;;  Emacsテクニックバイブルより ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14. org-mode
(require 'org)

;; 14.4 M-x org-remember
;; (org-remember-insinuate) ;; org-mode を最新にしたらエラーが出たのでコメントアウト2013/11/8
(setq org-directory "~/Dropbox/org/")
(setq org-mobile-directory "~/Dropbox/mobileorg/")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/mobile_todo.org")
(setq org-default-notes-file (expand-file-name "agenda/agenda.org" org-directory))
(setq org-remember-templates
      '(("Memo" ?m "** %?\n %i %a\n %t" nil "Inbox")
	;; ("Todo" ?t "** TODO %?\n %i %a\n %t" nil "Inbox")
	("Twitter" ?t "** %?\n %i %t" nil "Twitter")))
;; (global-set-key (kbd "C-,") 'org-remember)

;; 14.6 TODOリストを作成する
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCEL(c)")))

;; 14.14 予定表を見る
;; アジェンダ表示の対象ファイル
(setq org-agenda-files (list org-directory))
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-custom-commands
      '(("x" "My agenda view"
	 ((agenda)
	  (todo "TODO")
	  (tags-todo "movie")
	  (tags-todo "music")))))

;; Googleカレンダーへエスクポート
;; ネタ元
;; http://d.hatena.ne.jp/t0m0_tomo/20100103/1262537012
(setq org-combined-agenda-icalendar-file "~/Dropbox/calendar/org.ics")
(setq org-icalendar-include-todo t)
;; (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))
;; (setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(event-if-todo))
(setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))

(defun start-process-org ()
  (interactive)
  (start-process-shell-command "org-sync-gcal" "*org-sync-gcal*" "emacs" "--script" "~/Dropbox/.emacs.d/elisp/org-sync-gcal.el"))

(define-key global-map [f12] 'start-process-org)

;; Emacs 起動時に mobileorg から pull
(org-mobile-pull)

(if (linux?)
    (progn
      ;; (start-process-org)
      ;; Emacs 終了時に mobileorg に push
      (add-hook 'kill-emacs-hook 'org-mobile-push)))

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
   ;; elscreen で<C-tab>をタブの切り換えに割り当てたいので無効にする
   (define-key org-mode-map (kbd "<C-tab>") nil)
   (local-set-key (kbd "<M-return>") (lambda () (interactive)
                                       (if (not (eq (org-position-status (org-context)) :item))
                                           (progn
                                             (org-insert-heading-after-current)
                                             (insert (format "%s" "TODO [0/1] ")))
                                         (org-insert-heading))))
   (local-set-key (kbd "<M-S-return>") (lambda () (interactive)
                                         (message "%s" (org-context))))
   ;; 自作の M-return の動きに干渉するので key-combo-mode をオフにする
   (key-combo-mode -1)
   (auto-complete-mode t)
   ))

;; org-babel
(org-babel-do-load-languages    ;;; org7.5/doc/org.pdf p162
  'org-babel-load-languages
  '((R . t)
    (sh . t)
    (C . t)))

;; org ファイル読み込み時に自動的に画像をインライン表示する
;; 読み込み後、編集中の画像リンクには影響しない
(setq org-startup-with-inline-images t)


;; 常に画像を表示
;; リンク記述後 C-l で即表示
(add-hook 'org-mode-hook 'turn-on-iimage-mode)

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

;; ;;; helm
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
(require 'auto-install)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)
(setq auto-install-use-wget t)
(auto-install-compatibility-setup)
(setq auto-install-directory "~/Dropbox/.emacs.d/elisp/")
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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

;; 8.4 w3m.el
;; (if (linux?)
;;    (require 'w3m-load))

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

;;; ctags.el の設定(Emacs 実践入門 p191より)
(require 'ctags nil t)
(setq tags-revert-without-query t)
;; (setq ctags-command "ctags -e -R ")
;; ctagsを呼び出すコマンドライン
(setq ctags-command "ctags -R --fields=\"+afikKlmnsSzt\" ")
(global-set-key (kbd "<f5>") 'ctags-create-or-update-tags-table)
;; 定義ジャンプできるようにする
(when (require 'anything nil t)
  (require 'anything-exuberant-ctags)
  )
(global-set-key (kbd "C-;") (lambda () (interactive)
                              (ring-insert find-tag-marker-ring (point-marker))
                              (anything-exuberant-ctags-select-from-here)))
(global-set-key (kbd "C-.") (lambda () (interactive)
                              (ring-insert find-tag-marker-ring (point-marker))
                              (anything-exuberant-ctags-select)))

;; ジャンプ元に戻る
(global-set-key (kbd "C--") 'pop-tag-mark)


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

;; Zen Coding Mode
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(define-key zencoding-mode-keymap (kbd "C-z") 'zencoding-expand-yas)
(define-key zencoding-mode-keymap (kbd "<C-return>") nil)
(define-key zencoding-mode-keymap (kbd "<S-return>") 'zencoding-expand-line)

;;; コードの折り畳み
;; C coding style
(add-hook 'c-mode-hook
          '(lambda ()
    (hs-minor-mode 1)))
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

;;;;;; Lisp関連の設定
;; slime
(when (require 'slime nil t)
  (slime-setup '(slime-repl))

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

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

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

;; clojure
(require 'clojure-mode)
(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)

;; ciderのreplでauto-completeが使えるようにする
(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

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
  (define-key global-map (kbd "C-S-t") 'elscreen-create))
(put 'upcase-region 'disabled nil)

;最小の e2wm 設定例
(require 'e2wm)
(global-set-key (kbd "M-+") 'e2wm:start-management)
(global-set-key (kbd "M-=") 'e2wm:stop-management)
;; キーバインド
(e2wm:add-keymap
 e2wm:pst-minor-mode-keymap
 '(("<M-left>" . e2wm:dp-code) ; codeへ変更
   ("<M-right>"  . e2wm:dp-two)  ; twoへ変更
   ("<M-up>"    . e2wm:dp-doc)  ; docへ変更
   ("<M-down>"  . e2wm:dp-dashboard) ; dashboardへ変更
   ("C-."       . e2wm:pst-history-forward-command) ; 履歴を進む
   ("C-,"       . e2wm:pst-history-back-command) ; 履歴をもどる
   ("M-S"     . e2wm:my-toggle-sub) ; subの表示をトグルする
   ("prefix L"  . ielm)
   ("M-m"       . e2wm:pst-window-select-main-command)
   ) e2wm:prefix-key)

(defun e2wm:my-toggle-sub () ; Subをトグルする関数
  (interactive)
  (e2wm:pst-window-toggle 'sub t 'main))

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

;; https://github.com/kiwanami/emacs-window-manager/blob/master/e2wm-config.el
;; elscreen.el は e2wm と同じく、フレーム内のウインドウの挙動を監視し、
;; ウインドウの状態を保存、復帰している。そのため、スクリーン（タブ）
;; を作ると、e2wmはウインドウを管理できなくなる。デフォルトの状態では、
;; スクリーンを複数作らなければelscreen.el自体を動かすことについては
;; 特に問題はない。

;; 回避方法としては、elscreenの管理オブジェクトの中にe2wmの管理オブジェ
;; クトを入れてelscreenの傘下に入るという方法を行う。これにより、
;; elscreenごとに異なる異なるe2wmインスタンスを持つ頃が出来る。
;; （e2wmの所々でグローバルで値を共有しているところがあるので今後直す）

(eval-after-load "elscreen"
  '(progn
     ;; overrides storages for elscreen
     (defadvice e2wm:frame-param-get (around e2wm:ad-override-els (name &optional frame))
       ;; frame is not used...
       (e2wm:message "** e2wm:frame-param-get : %s " name)
       (let ((alst (cdr (assq 'e2wm-frame-prop
                              (elscreen-get-screen-property
                               (elscreen-get-current-screen))))))
         (setq ad-return-value (and alst (cdr (assq name alst))))))
     (defadvice e2wm:frame-param-set (around e2wm:ad-override-els (name val &optional frame))
       (e2wm:message "** e2wm:frame-param-set : %s / %s" name val)
       (let* ((screen (elscreen-get-current-screen))
              (screen-prop (elscreen-get-screen-property screen))
              (alst (cdr (assq 'e2wm-frame-prop screen-prop))))
         (set-alist 'alst name val)
         (set-alist 'screen-prop 'e2wm-frame-prop alst)
         (elscreen-set-screen-property screen screen-prop)
         (setq ad-return-value val)))
     ;; grab switch events
     (defun e2wm:elscreen-define-advice (function)
       (eval
        `(defadvice ,function (around e2wm:ad-override-els)
           (e2wm:message "** %s vvvv" ',function)
           (when (e2wm:managed-p)
             (e2wm:message "** e2wm:managed")
             (let ((it (e2wm:pst-get-instance)))
               (e2wm:pst-method-call e2wm:$pst-class-leave it (e2wm:$pst-wm it)))
             (e2wm:pst-minor-mode -1))
           (e2wm:message "** ad-do-it ->")
           ad-do-it
           (e2wm:message "** ad-do-it <-")
           (e2wm:message "** e2wm:param %s"
                         (cdr (assq 'e2wm-frame-prop
                                    (elscreen-get-screen-property
                                     (elscreen-get-current-screen)))))
           (when (e2wm:managed-p)
             (e2wm:message "** e2wm:managed")
             (let ((it (e2wm:pst-get-instance)))
               (e2wm:pst-method-call e2wm:$pst-class-start it (e2wm:$pst-wm it)))
             (e2wm:pst-minor-mode 1))
           (e2wm:message "** %s ^^^^^" ',function)
           )))
     (defadvice elscreen-create (around e2wm:ad-override-els)
       (let (default-wcfg)
         (when (e2wm:managed-p)
           (loop for screen in (reverse (sort (elscreen-get-screen-list) '<))
                 for alst = (cdr (assq 'e2wm-frame-prop
                                       (elscreen-get-screen-property screen)))
                 for wcfg = (and alst (cdr (assq 'e2wm-save-window-configuration alst)))
                 if wcfg
                 do (setq default-wcfg wcfg) (return)))
         ad-do-it
         (when default-wcfg
           (set-window-configuration default-wcfg))))
     (defadvice elscreen-run-screen-update-hook (around e2wm:ad-override-els)
       (flet ((e2wm:managed-p () nil))
         ad-do-it
         ))

     ;; apply defadvices to some elscreen functions
     (loop for i in '(elscreen-goto
                      elscreen-kill
                      elscreen-clone
                      elscreen-swap)
           do (e2wm:elscreen-define-advice i))
     (defun e2wm:elscreen-override ()
       (ad-activate-regexp "^e2wm:ad-override-els$")
       (setq e2wm:override-window-ext-managed t))
     (defun e2wm:elscreen-revert ()
       (ad-deactivate-regexp "^e2wm:ad-override-els$")
       (setq e2wm:override-window-ext-managed nil))
     ;; start and exit
     (add-hook 'e2wm:pre-start-hook 'e2wm:elscreen-override)
     (add-hook 'e2wm:post-stop-hook 'e2wm:elscreen-revert)
     ))

;;; 自作 ツールの読み込み
(require 'keep-household-accounts)
