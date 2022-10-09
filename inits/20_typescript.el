;; typescript
(straight-use-package 'typescript-mode)
(straight-use-package 'tide)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'typescript-mode-hook
          '(lambda ()
             (interactive)
             (tide-setup)
             (flycheck-mode +1)
             (tide-hl-identifier-mode +1)
             (company-mode +1)
             (eldoc-mode +1)
             (local-set-key (kbd "C-o") 'tide-jump-back)
             (local-set-key (kbd "C-]") 'tide-jump-to-definition)
             (local-set-key (kbd "C-M-r") 'tide-references)
             ))
