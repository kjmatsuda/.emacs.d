(use-package lsp-mode
  ;; :ensure t にすることで、自動的にインストールされる
  :ensure t
  :defer t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          ;; (lsp)
                          )))
;; optionally
(use-package lsp-ui
  :after (lsp-mode)
  :ensure t
  :commands lsp-ui-mode)

;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list)

(if (not (is-termux))
    ;; key bindings
    (with-eval-after-load 'lsp-treemacs
      (define-key lsp-mode-map (kbd "C-M-c") 'lsp-treemacs-call-hierarchy)
      )
  ;; key bindings
  (with-eval-after-load 'lsp-treemacs
      (define-key lsp-mode-map (kbd "M-c") 'lsp-treemacs-call-hierarchy)
    )
)

;; optionally if you want to use debugger
(use-package dap-mode
  :after (lsp-mode)
  :ensure t
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode) (python-mode . dap-mode))
  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python")))
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

;; ;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))
