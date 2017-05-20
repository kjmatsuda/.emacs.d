;; scheme 用の設定
(if (win?)
    (progn (setq process-coding-system-alist
                 (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
           (setq scheme-program-name "gosh -i")
           (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
           (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t))
  (setq scheme-program-name "gosh"))

(require 'cmuscheme)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cS" 'scheme-other-window)

;; カーソルのあるポイントのinfoを引く
(defvar anything-c-source-info-gauche-refj
   ;; '((info-index . "~/../gauche/share/info/gauche-refj.info")))
  '((info-index . "gauche-refj.info")))
(defun anything-info-ja-at-point ()
  "Preconfigured `anything' for searching info at point."
  (interactive)
  (anything '(anything-c-source-info-gauche-refj)
            (thing-at-point 'symbol) nil nil nil "*anything info*"))
(define-key global-map (kbd "C-M-i") 'anything-info-ja-at-point)
