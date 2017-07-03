;;;;;; Lisp関連の設定
;; slime
(when (require 'slime nil t)
  (slime-setup '(slime-repl slime-fancy slime-banner))
 
  ;; sbclのパスを記述
  (if (win?)
      ;; (setq inferior-lisp-program "sbcl.exe")
      (load (expand-file-name "~/.roswell/helper.el"))
    (setq inferior-lisp-program "sbcl"))

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
    ))


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

;; Hyperspec の検索開始キー
(global-unset-key (kbd "C-o"))
(global-set-key (kbd "C-o") 'hyperspec-lookup)
