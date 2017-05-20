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
