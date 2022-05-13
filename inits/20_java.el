(add-hook 'java-mode-hook 'highlight-symbol-mode)
(add-hook 'java-mode-hook #'electric-spacing-mode)
(add-to-list 'auto-mode-alist '("\\.jack\\'" . java-mode))
