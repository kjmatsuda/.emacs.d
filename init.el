(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                     ("marmalade" . "http://marmalade-repo.org/packages/")
                     ("melpa" . "http://melpa.org/packages/")))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
(init-loader-load "~/.emacs.d/inits")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-highlight-search t)
 '(ag-reuse-buffers (quote nil))
 '(ag-reuse-window (quote nil))
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(imenu-list-size 0.2)
 '(initial-buffer-choice (quote ignore))
 '(neo-click-changes-root t)
 '(org-agenda-format-date "%Y/%m/%d (%a)")
 '(package-selected-packages
   (quote
    (web-mode js2-mode tern tern-auto-complete python-mode init-loader org-mobile-sync alda-mode slime macrostep auto-complete neotree elscreen-persist elscreen-buffer-group package-utils persp-mode window-layout helm-ag cdb ccc ddskk helm-core popup async helm helm-projectile helm-gtags gtags inf-clojure ripgrep todotxt-mode ruby-block quickrun melpa key-combo helm-migemo helm-descbinds flymake flycheck emmet-mode elscreen ctags clojure-cheatsheet ac-nrepl)))
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
