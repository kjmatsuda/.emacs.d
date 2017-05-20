;; html や css の編集に便利な emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(setq emmet-move-cursor-between-quotes t) ;; default nil

;; emmet-mode を入れたので zencoding-mode はコメントアウトする
;; ;; Zen Coding Mode
;; (require 'zencoding-mode)
;; (add-hook 'sgml-mode-hook 'zencoding-mode)
;; (define-key zencoding-mode-keymap (kbd "C-z") 'zencoding-expand-yas)
;; (define-key zencoding-mode-keymap (kbd "<C-return>") nil)
;; (define-key zencoding-mode-keymap (kbd "<S-return>") 'zencoding-expand-line)
