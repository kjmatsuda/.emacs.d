;; plantuml-mode
(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.pu\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.plu\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

(autoload 'plantuml-mode "plantuml-mode" "PlantUML mode" t)

(setq plantuml-executable-path "plantuml")
(setq plantuml-default-exec-mode 'executable)
(setq plantuml-output-type "png")

(defun plantuml-preview-frame (prefix)
  (interactive "p")
  (plantuml-preview 16))

(add-hook 'plantuml-mode-hook
          (lambda ()
            (define-key plantuml-mode-map (kbd "C-c C-p") 'plantuml-preview-frame)
            (define-key plantuml-mode-map (kbd "C-c C-s") 'plantuml-execute)
            (setq plantuml-executable-args
                  (append plantuml-executable-args '("-charset" "UTF-8")))))

(defun plantuml-execute ()
  (interactive)
  (when (buffer-modified-p)
    (map-y-or-n-p "Save this buffer before executing PlantUML?"
                  'save-buffer (list (current-buffer))))
  (let ((code (buffer-string))
        out-file
        cmd)
    (when (string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s*$" code)
      (setq out-file (match-string 1 code)))
    (setq cmd (concat
               "plantuml"
               (and out-file (concat "-t" (file-name-extension out-file))) " "
               plantuml-options " "
               (buffer-file-name)))
    (message cmd)
    (shell-command cmd)
    
    (message "done")))

(setq plantuml-options "-charset UTF-8")
