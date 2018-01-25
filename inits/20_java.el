;;;;;; Java
(add-hook 'java-mode-hook
      '(lambda ()
         (key-combo-define-local (kbd "(") '("(`!!')"))
         (key-combo-define-local (kbd "()") "()")
         (key-combo-define-local (kbd "((") "((`!!'))")
         (key-combo-define-local (kbd "(\"") "(\"`!!'\")")
         (key-combo-define-local (kbd "\"") '("\"`!!'\""))
         (key-combo-define-local (kbd "\"\"") "\"\"")
         (key-combo-define-local (kbd "{") '("{`!!'}"))
         (key-combo-define-local (kbd "{}") "{}")
         (key-combo-define-local (kbd "[") '("[`!!']"))
         (key-combo-define-local (kbd "[]") "[]")
         ))

(add-hook 'java-mode-hook 'highlight-symbol-mode)
