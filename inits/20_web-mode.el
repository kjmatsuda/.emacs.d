(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
'(("php"    . "\\.phtml\\'")
  ("blade"  . "\\.blade\\.")))

(setq web-mode-auto-close-style 1)
(setq web-mode-tag-auto-close-style t)
(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)

;; html や css の編集に便利な emmet-mode
(use-package emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(setq emmet-move-cursor-between-quotes t) ;; default nil

(add-hook 'web-mode-hook
          '(lambda ()
             ;; HTML を編集するときに company-mode の補完が邪魔なのでオフにする
             (company-mode -1)
             )
          )

(add-hook 'css-mode-hook 'ac-emmet-css-setup)
(add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
(add-hook 'html-mode-hook 'ac-emmet-html-setup)
(add-hook 'web-mode-hook 'ac-emmet-html-setup)

(add-hook 'sgml-mode-hook 'auto-complete-mode)
(add-hook 'html-mode-hook 'auto-complete-mode)
(add-hook 'css-mode-hook  'auto-complete-mode)
(add-hook 'web-mode-hook  'auto-complete-mode)

(add-hook 'auto-complete-mode-hook
          (lambda ()
            (define-key ac-completing-map "\C-n" 'ac-next)
            (define-key ac-completing-map "\C-p" 'ac-previous)
            (make-local-variable 'ac-ignore-case)
            (setq ac-ignore-case nil)))
