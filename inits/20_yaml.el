(straight-use-package
 '(call-graph :type git :host github :repo "yoshiki/yaml-mode"))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

(add-hook 'yaml-mode-hook
  '(lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent)
     ;; Yaml のときは 補完候補の大文字、小文字を区別したいので、company-mode を無効にして
     ;; auto-complete-mode を使う
     (company-mode -1)
     ))

(add-hook 'yaml-mode-hook  'auto-complete-mode)

(add-hook 'auto-complete-mode-hook
          (lambda ()
            (define-key ac-completing-map "\C-n" 'ac-next)
            (define-key ac-completing-map "\C-p" 'ac-previous)
            (make-local-variable 'ac-ignore-case)
            (setq ac-ignore-case nil)))
