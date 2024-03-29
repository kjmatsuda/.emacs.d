(autoload 'python-mode "python-mode" "Python editing mode." t)
(custom-set-variables
  '(py-indent-offset 4)
  )

(use-package python-black
  :demand t
  :after python
  :hook nil)

(add-hook 'python-mode-hook
  '(lambda()
    (setq tab-width 4) 
    (setq indent-tabs-mode nil)
    ;; 以下をしないと、繰り返しhelp画面が表示されて使いものにならなかった
    (setq-local eldoc-documentation-function  nil)

    ;; 以下をしないと、Termux (Emacs 28)では勝手に補完されてまともに編集できなかった
    (if (is-termux)
        (progn
          (delete 'company-capf company-backends)
          )
      )
    (when (executable-find "black")
      (local-set-key (kbd "C-x C-s") 'python-black-buffer))
  )
)

(add-hook 'python-mode-hook #'electric-spacing-mode)

