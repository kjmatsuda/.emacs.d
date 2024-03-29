
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook
    (lambda ()
      (tern-mode t)
      (when (executable-find "prettier")
        (local-set-key (kbd "C-x C-s") 'my/prettier))
      (make-local-variable 'js-indent-level)
      (setq js-indent-level 2)
      ;; (setq add-node-modules-path-debug t)
      ))

(use-package indium
  :config
  (add-hook 'js-mode-hook #'indium-interaction-mode)
  )

(add-hook 'js2-mode-hook #'electric-spacing-mode)

;; ./node_modules/.bin/ の下のバイナリ(prettier)を優先的に使用するために add-node-modules-path を導入
(eval-after-load 'js2-mode
  '(add-hook 'js2-mode-hook #'add-node-modules-path))

;; company関連
(setq company-tern-property-marker "")
(defun company-tern-depth (candidate)
  "Return depth attribute for CANDIDATE. 'nil' entries are treated as 0."
  (let ((depth (get-text-property 0 'depth candidate)))
    (if (eq depth nil) 0 depth)))
(add-hook 'js2-mode-hook 'tern-mode) ; 自分が使っているjs用メジャーモードに変える
(add-to-list 'company-backends 'company-tern) ; backendに追加









