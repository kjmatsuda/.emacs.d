;; scheme 用の設定
(if (win?)
    (progn (setq process-coding-system-alist
                 (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
           (setq scheme-program-name "gosh -i")
           (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
           (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t))
  (setq scheme-program-name "gosh"))

(use-package cmuscheme)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cg" 'scheme-other-window)

(add-hook 'scheme-mode-hook 'highlight-symbol-mode)
;; (add-hook 'scheme-mode-hook #'electric-spacing-mode)
