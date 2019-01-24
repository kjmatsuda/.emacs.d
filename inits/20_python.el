(autoload 'python-mode "python-mode" "Python editing mode." t)
(custom-set-variables
  '(py-indent-offset 4)
)
(add-hook 'python-mode-hook
  '(lambda()
    (setq tab-width 4) 
    (setq indent-tabs-mode nil)
    ;; 以下をしないと、繰り返しhelp画面が表示されて使いものにならなかった
    (setq-local eldoc-documentation-function  nil)
  )
)

(add-hook 'python-mode-hook #'electric-spacing-mode)
