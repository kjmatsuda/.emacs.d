
;; Termux (Emacs28)で package-desc-vers が見つからないというエラーが出たことへの対策
(fset 'package-desc-vers 'package--ac-desc-version)

;; Termux (Emacs28)で Package cl is deprecated が出ないようにする対策
(setq byte-compile-warnings '(cl-function))

;;Emacs @28用void: browse-url-mosaic-program対策
(setq browse-url-mosaic-program nil)

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                     ("marmalade" . "http://marmalade-repo.org/packages/")
                     ("melpa" . "http://melpa.org/packages/")))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

(require 'use-package)

;; straight.el自身のインストールと初期設定を行ってくれる
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-packageをインストールする
(straight-use-package 'use-package)

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

;; Termuxか否かを判別する
(defvar which-bash-str "")
(setq which-bash-str (shell-command-to-string "which bash"))

(defun is-termux()
  (if (string-match "termux" which-bash-str)
      t
    nil))

;; オプションなしで自動的にuse-packageをstraight.elにフォールバックする
;; 本来は (use-package hoge :straight t) のように書く必要がある
;; (setq straight-use-package-by-default t)


(use-package init-loader)

(init-loader-load "~/.emacs.d/inits")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(ag-reuse-buffers (quote nil))
 '(ag-reuse-window (quote nil))
 '(cg-initial-max-depth 5)
 '(cg-search-filters (quote ("grep -E \"\\.(cpp|cc|c|el):\"")))
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(imenu-list-size 0.2)
 '(initial-buffer-choice (quote ignore))
 '(neo-click-changes-root t)
 '(org-agenda-format-date "%Y/%m/%d (%a)")
 '(package-selected-packages
   (quote
    (bind-key use-package pythonic anaconda-mode call-graph tree-mode ivy hierarchy image+ company-tern company-emoji dash-functional org-present indium company js2-refactor multiple-cursors electric-spacing web-mode js2-mode tern tern-auto-complete python-mode init-loader org-mobile-sync alda-mode slime macrostep auto-complete neotree elscreen-persist elscreen-buffer-group package-utils persp-mode window-layout helm-ag cdb ccc ddskk helm-core popup async helm helm-projectile helm-gtags gtags inf-clojure ripgrep todotxt-mode ruby-block quickrun melpa key-combo helm-migemo helm-descbinds flymake flycheck emmet-mode elscreen ctags clojure-cheatsheet ac-nrepl)))
 '(py-indent-offset 4)
 '(right-click-context-global-menu-tree
   (quote
    (("Tags"
      ("Jump" :call
       (my/helm-gtags-find-tag-from-here))
      ("Reference" :call
       (helm-gtags-find-rtag))
      ("Pop" :call
       (helm-gtags-pop-stack)))
     ("View"
      ("Tree" :call
       (neotree-toggle))
      ("imenu" :call
       (imenu-list-smart-toggle)))
     ("File"
      ("Open" :call nil))
     ("Buffer"
      ("Select" :call nil))
     ("Quit" :call nil)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-date ((t :weight bold))))

;; Because insert-string is not available in Emacs26.
(defalias 'insert-string 'insert)
