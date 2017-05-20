;; plantuml-mode
(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
(autoload 'plantuml-mode "plantuml-mode" "PlantUML mode" t)

(add-hook 'plantuml-mode-hook
          (lambda () (auto-complete-mode t)))

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
               "java -jar " plantuml-java-options " "
               (shell-quote-argument plantuml-jar-path) " "
               (and out-file (concat "-t" (file-name-extension out-file))) " "
               plantuml-options " "
               (buffer-file-name)))
    (message cmd)
    (shell-command cmd)
    (message "done")))

(if (linux?)
    (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (setq plantuml-jar-path "C:/opt/jars/plantuml.jar")
  )
(setq plantuml-java-options "")
(setq plantuml-options "-charset UTF-8")
(setq plantuml-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c") 'plantuml-execute)
        map))
