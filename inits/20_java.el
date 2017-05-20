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
