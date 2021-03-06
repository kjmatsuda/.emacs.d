;;;;;; PHPに関する設定
;; php-mode
(use-package php-mode)
(add-hook 'php-mode-user-hook
	  '(lambda ()
	     (setq tab-width 2)
	     (setq indent-tabs-mode nil)))
;(setq php-mode-force-pear t) ;PEAR規約のインデント設定にする

;; php-completion
(add-hook 'php-mode-hook
         (lambda ()
             (use-package php-completion)
             (php-completion-mode t)
             (define-key php-mode-map (kbd "\C-co") 'phpcmp-complete)))

(add-hook 'php-mode-hook #'electric-spacing-mode)
