;;;;;; Ruby関連の設定
;;; 参考
;;  1) http://futurismo.biz/archives/2213
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; (use-package ruby-electric)
;; (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
;; (setq ruby-electric-expand-delimiters-list nil)

;; ruby-block.el --- highlight matching block
(use-package ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

(add-hook 'ruby-mode-hook 'highlight-symbol-mode)
(add-hook 'ruby-mode-hook #'electric-spacing-mode)

;; TODO Rubyでタグジャンプ
