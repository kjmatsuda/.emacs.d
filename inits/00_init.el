; melpa.el
(use-package melpa)

;; emacs 設定ファイル
(use-package cl)
;;;;;;;;;;;;;;;;;; 初期処理 ;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs26 では default-*関連の変数が削除されたようなので、定義しておく
(if (string-match "26" emacs-version)
  (progn (setq default-fill-column (default-value 'fill-column))
         (setq default-tab-width 4))
  )

;; load-pathをサブディレクトリごと追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

(when (win?)
  (add-to-list 'load-path "C:/opt/emacs/site-lisp/apel")
  (add-to-list 'load-path "C:/opt/emacs/site-lisp/emu")
  (setq org-mobile-checksum-binary "~/.emacs.d/other/sha1sum.exe"))

;; cua-mode の設定
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; 'pop-mark' C-u C-SPC C-SPC C-SPC... のように
;; C-SPC を連続で入力するだけで，連続でマークを辿れるようになる
(setq set-mark-command-repeat-pop t)

;; バックアップファイルを作成しない
(setq make-backup-files nil)
;; オートセーブファイルを作らない
(setq auto-save-default nil)

;; M-wやC-kでコピーしたものを、他のアプルケーションで貼り付け可能にする
(cond (window-system
       (setq x-select-enable-clipboard t)
))


;; 2017/12/18(Mon) スマホのtermux上のEmacsではkey-comboが有効だと
;; altキーが正常に効かなくなる
;; よって、(electric-pair-mode 1)で代用する
(electric-pair-mode 1)

;; 英和辞書
(use-package sdic
  :config
  (global-set-key "\C-cw" 'sdic-describe-word)
  (global-set-key "\C-cp" 'sdic-describe-word-at-point))
;; 動作と見掛けを調節するための設定
(setq sdic-window-height 10
      sdic-disable-select-window t)
(if (win?)
    (progn
      ;; 使用する辞書ファイルの設定
      (setq sdic-eiwa-dictionary-list '((sdicf-client "~/.emacs.d/dict/gene.sdic")))
      (setq sdic-waei-dictionary-list '((sdicf-client "~/.emacs.d/dict/jedict.sdic")))
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

;;;; フランス語の入力切り換え
(defun toggle-input-method-french ()
  (interactive)
  (if (equal current-input-method "french-postfix")
      (set-input-method nil)
    (set-input-method "french-postfix")))

(global-set-key (kbd "C-x f") 'toggle-input-method-french)

;; wdired
;; http://at-aka.blogspot.com/2006/12/emacs-dired-wdired.html
;; (eval-after-load "dired"
;;   '(lambda ()
;;      (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)))
(use-package dired)
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

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "s" 'dired-various-sort-change-or-edit)
             ))


;; ;; Visual Basicモード
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

;; helm
(use-package helm-config)
(use-package helm
  :bind (
         ("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("M-y" . helm-show-kill-ring)
         ("M-i" . helm-imenu)
         ;; Termux (Emacs28)で C-x b を実行したら、"No buffer named *temp*"と表示されて、バッファ切り替えができないので無効化
         ;; ("C-x b" . helm-buffers-list)
         ;; ("C-M-h" . helm-apropos)
         ("C-c C-f" . find-function)
         ("C-c o" . helm-occur)
         ("C-c m" . helm-man-woman)
         :map helm-map
         ("C-h" . delete-backward-char)
         :map helm-find-files-map
         ("C-h" . delete-backward-char)
         ("TAB" . helm-execute-persistent-action)
         :map helm-read-file-map
         ("TAB" . helm-execute-persistent-action)
         )
  :config
  (helm-mode 1)
  )

;; helmの操作感を改善するため、下記の設定を追加
;; https://abicky.net/2014/01/04/170448/
;; (define-key global-map (kbd "M-r")     'helm-resume)

;; 存在しないファイル名を指定してTABを入力した際にバッファを生成しないようにする
(defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  "Execute command only if CANDIDATE exists"
  (when (file-exists-p candidate)
    ad-do-it))

;; Helm interface improvement. Stop asking "File does not exist, create buffer?" every time, when i try to create new file.
(setq helm-ff-newfile-prompt-p nil)

;; 2.1 ddskk
(defun skk-latin-toggle()
  (interactive)
  (if skk-mode
      (if skk-latin-mode
          (skk-mode t)
        (progn
          (skk-latin-mode t)
          )))
  )

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

(setq skk-large-jisyo "~/.emacs.d/skk-jisyo/SKK-JISYO.L+emoji.utf8")
(setq skk-jisyo "~/.skk-jisyo.utf8")
(setq skk-jisyo-code 'utf-8)

;; 4.2 uniquify.el
(use-package uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniqify-ignore-buffers-re "*[^*]+*")

;; 4.4 recentf.el
;; (setq recentf-max-saved-items 500)
;; (setq recentf-exclude '("/TAGS$" "/var/tmp/"))
(use-package recentf-ext)
;; (define-key global-map (kbd "C-x f") 'recentf-open-files)

;; ファイルを自動保存する
(use-package auto-save-buffers-enhanced
  :straight t
  :config
   ;;; 1秒後に保存
  (setq auto-save-buffers-enhanced-interval 1)
  ;;; 特定のファイルのみ有効にする
  (setq auto-save-buffers-enhanced-include-regexps '(".+")) ;全ファイル
  ;; not-save-fileと.ignoreは除外する
  (setq auto-save-buffers-enhanced-exclude-regexps '("^not-save-file" "\\.ignore$"))
   ;;; Wroteのメッセージを抑制
  (setq auto-save-buffers-enhanced-quiet-save-p t)
  ;;; *scratch*も ~/.emacs.d/scratch に自動保存
  (setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
  (setq auto-save-buffers-enhanced-file-related-with-scratch-buffer
        (locate-user-emacs-file "scratch"))
  (auto-save-buffers-enhanced t))

;; (when (executable-find "cmigemo")
;;   ;; 5.5 migemo.el
;;   (setq migemo-command "cmigemo")
;;   (setq migemo-options '("-q" "--emacs" "-i" "\g"))
;;   (setq migemo-user-dictionary nil)
;;   (setq migemo-regex-dictionary nil)
;;   ;; キャッシュ機能を利用する
;;   (setq migemo-use-pattern-alist t)
;;   (setq migemo-use-frequent-pattern-alist t)
;;   (setq migemo-pattern-alist-length 1024)
;;   ;; 辞書の文字コードを指定．
;;   (if (linux?)
;;       (setq migemo-coding-system 'utf-8-unix)
;;     (setq migemo-coding-system 'cp932-unix))
;;   (load-library "migemo")
;;   (migemo-init)
;; )

;; (when (not (executable-find "cmigemo"))
;;   (setq migemo-isearch-enable-p nil))

;; 5.6 point-undo.el
;; カーソル位置を戻す
;; (use-package point-undo)
;; (define-key global-map (kbd "C--") 'point-undo)
;; (define-key global-map (kbd "C-=") 'point-redo)

;; 6.6 yasnippet.el
(use-package yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/mysnippets"   ;; 自分で追加するスニペット
        "~/.emacs.d/elpa/yasnippet-snippets-20180122.521/snippets"
        ))

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(yas-global-mode 1)

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

;;;;;;;;;;;;;;;;;  VCS関連 ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit)

(defun my-magit-push (source target args)
  "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer."
  (interactive
   (let ((source (magit-read-local-branch-or-commit "Push")))
     (list source
           (magit-read-remote-branch
            (format "Push %s to" source) nil
            (if (magit-local-branch-p source)
                (or (magit-get-push-branch source)
                    (magit-get-upstream-branch source))
              (and (magit-rev-ancestor-p source "HEAD")
                   (or (magit-get-push-branch)
                       (magit-get-upstream-branch))))
            source 'confirm)
           (magit-push-arguments))))
  (magit-git-push source target args))

(global-set-key (kbd "C-x g s") 'magit-status)
(global-set-key (kbd "C-x g p") 'magit-pull)
(global-set-key (kbd "C-x g P") 'my-magit-push)


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

;;; Enable helm-gtags-mode
(use-package helm-gtags)
(setq helm-gtags-auto-update t)
(setq helm-gtags-update-interval-second 20)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'clojure-mode-hook 'helm-gtags-mode)
(add-hook 'emacs-lisp-mode-hook 'helm-gtags-mode)
(add-hook 'lisp-mode-hook 'helm-gtags-mode)
(add-hook 'js-mode-hook 'helm-gtags-mode)
(if (is-termux)
    (add-hook 'typescript-mode-hook 'helm-gtags-mode)
  )
(add-hook 'python-mode-hook 'helm-gtags-mode)

;;;; Code Formatter
;; 引用元
;; Emacs + Prettier = ❤️ - Qiita
;; https://qiita.com/ybiquitous/items/0761feeff7f31ba0a476
(defun my/prettier ()
  (interactive)
  (shell-command
    (format "%s --write %s"
      (shell-quote-argument (executable-find "prettier"))
      (shell-quote-argument (expand-file-name buffer-file-name))))
  (revert-buffer t t t))

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
    (init-loader org-mobile-sync alda-mode slime macrostep neotree elscreen-persist elscreen-buffer-group package-utils persp-mode window-layout helm-ag cdb ccc ddskk helm-core popup async helm helm-projectile helm-gtags gtags inf-clojure ripgrep todotxt-mode ruby-block quickrun melpa helm-migemo helm-descbinds flymake flycheck emmet-mode elscreen ctags clojure-cheatsheet ac-nrepl))))

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

(with-eval-after-load 'helm-gtags
  (define-key helm-gtags-mode-map (kbd "C-M-;") 'my/find-tag-without-ns)
  (define-key helm-gtags-mode-map (kbd "C-]") 'my/helm-gtags-find-tag-from-here)
  (define-key helm-gtags-mode-map [M-left] 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map [M-right] 'helm-gtags-next-history)
  (define-key helm-gtags-mode-map (kbd "C--") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-o") 'helm-gtags-pop-stack))

;; 今開いているファイルの org 用リンクをコピーする(dired-mode ではカーソルの当たっているファイルのリンクがコピーされる)
(global-set-key (kbd "C-c C-w") 'org-store-link)

;; org-mode から orgファイル以外に飛んだ際に元の org ファイルに簡単に戻れるようにする
(global-set-key (kbd "C-o") 'org-mark-ring-goto)
(with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-c C-o") 'org-mark-ring-goto)
   )

(if (not (is-termux))
    ;; key bindings
    (with-eval-after-load 'helm-gtags
      (define-key helm-gtags-mode-map (kbd "C-M-r") 'helm-gtags-find-rtag)
      (define-key helm-gtags-mode-map (kbd "C-M-s") 'helm-gtags-find-symbol)
      )
  ;; key bindings
  (with-eval-after-load 'helm-gtags
    (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
    (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
    )
  )


;; (use-package helm-ag)
;; ; agのデフォルトのコマンドオプションを指定
;; ; -nを消すとサブディレクトリも再帰的に検索します
;; (setq helm-ag-base-command "ag --nocolor --nogroup")
;; (define-key global-map (kbd "M-O") 'helm-ag)
;; (define-key global-map (kbd "M-o") 'helm-ag-this-file)
;; (define-key global-map (kbd "M-G") 'ag)
;; ;; 現在位置のシンボルをデフォルトで検索語句に設定する
;; (setq helm-ag-insert-at-point 'symbol)

(use-package ripgrep)

(if (not (is-termux))
    (progn
      (define-key global-map (kbd "C-H") 'ripgrep-regexp)
      )
  (progn
    (define-key global-map (kbd "M-h") 'ripgrep-regexp)
    )
  )

;; eでwgrepモードにする
(setf wgrep-enable-key "r")
;; wgrep終了時にバッファを保存
(setq wgrep-auto-save-buffer t)
;; read-only bufferにも変更を適用する
(setq wgrep-change-readonly-file t)

;; call-graph
(straight-use-package
 '(call-graph :type git :host github :repo "kjmatsuda/call-graph"))

(use-package call-graph
  :config
  (custom-set-variables
   '(cg-initial-max-depth 5)
   '(cg-search-filters '("grep -E \"\\.(cpp|cc|c|el|clj):\""))
  )
  (call-graph) ;; to launch it
  )

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
(use-package flymake)

;; GUIの警告は表示しない
(setq flymake-gui-warnings-enabled nil)

;; ;; 全てのファイルで flymakeを有効化
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

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

;;;; 単語補完
(use-package pabbrev)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(if (is-termux)
    (progn
      (use-package info)
      (setq Info-default-directory-list
            (cons (expand-file-name "~/.emacs.d/info/")
                  Info-default-directory-list))
      )
  )

;; (when (load "btc-ticker" t)
;;   (use-package btc-ticker)

;;   ;;Optional: You can setup the fetch interval
;;   ;;default: 10 secs
;;   (setq btc-ticker-api-poll-interval 10)

;;   ;;Enable btc-ticker-mode
;;   (btc-ticker-mode 1))
