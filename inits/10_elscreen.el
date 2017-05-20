;; ElScreen
(when (require 'elscreen nil t)
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
    (define-key elscreen-map (kbd "C-z") 'suspend-emacs))
  (define-key global-map [\C-\S-\iso-\lefttab] 'elscreen-previous)
  (define-key global-map [\C-\S-\tab] 'elscreen-previous)
  (define-key global-map [\C-\tab] 'elscreen-next)
  (define-key global-map (kbd "C-S-t") 'elscreen-create)
  (elscreen-start)
  ;; (elscreen-separate-buffer-list-mode 1)
  ;; (elscreen-persist-mode 1)
  )
(put 'upcase-region 'disabled nil)
