
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook
    (lambda ()
        (tern-mode t)))

(eval-after-load 'tern
    '(progn
        (require 'tern-auto-complete)
        (tern-ac-setup)))


(if (is-termux)
    (progn
      (add-hook 'js2-mode-hook #'electric-spacing-mode)))










