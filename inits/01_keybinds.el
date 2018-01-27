;; フレームの操作性を向上する
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(defun select-buffer-start-with (b-list buff-name-top)
  (if (eq b-list nil)
      nil
    (if (eq (string-match (concat "^" buff-name-top) (buffer-name (car b-list))) 0)
        ;; バッファ名が一致した
        (switch-to-buffer (car b-list))
      (select-buffer-start-with (cdr b-list) buff-name-top))))

(defun exists-visible-buffer-start-with (b-list buff-name-start-with)
  (if (eq b-list nil)
      nil
    (if (and (eq (string-match (concat "^" buff-name-start-with) (buffer-name (car b-list))) 0) (get-buffer-window (buffer-name (car b-list)) t))
        ;; バッファ名が一致した、かつバッファが表示されている
        t
      (exists-visible-buffer-start-with (cdr b-list) buff-name-start-with))))

;; TODO サブウィンドウのバッファ名を"*slime-repl"固定にしているが、用途に応じて変えるようにする
;; サブウィンドウを構成する
(defun subwindow-show ()
  (interactive)
  (if (not (exists-visible-buffer-start-with (buffer-list) "*slime-repl"))
      (progn
        ;; 表示されていない場合
        (neotree-hide)             ;; neotreeかくす
        (imenu-list-minor-mode -1) ;; imenuかくす
        (split-window-vertically (/ (* 2 (window-height)) 3))
        (other-window 1)
        (select-buffer-start-with (buffer-list) "*slime-repl"))))

(defun subwindow-hide ()
    (interactive)
    (if (exists-visible-buffer-start-with (buffer-list) "*slime-repl")
        (progn
          (if (not (string-match (concat "^" "*slime-repl") (buffer-name (current-buffer))))
              ;; 現在、サブウィンドウにフォーカスがない場合
              (other-window 1))
          (delete-window))))

(defun subwindow-toggle ()
  (interactive)
  (if (exists-visible-buffer-start-with (buffer-list) "*slime-repl")
      ;; 表示中の場合
      (subwindow-hide)
    ;; 非表示の場合
    (subwindow-show)))

(defun show-sub-window-for-develop ()
  (interactive)
  ;; (delete-other-windows)
  ;; (other-window 1)
  (split-window-vertically (/ (* 2 (window-height)) 3))
  (other-window 1)
  (if (eq major-mode 'clojure-mode)
      (select-buffer-start-with (buffer-list) "*cider-repl")
    (if (eq major-mode 'lisp-mode)
        (select-buffer-start-with (buffer-list) "*slime-repl")
      ;; それ以外のモードの場合
      (progn
        (shell)
        (select-buffer-start-with (buffer-list) "*shell*"))))
  ;; (other-window 1)
  ;; (other-window 1)
  )

;; C-S-oで次のwindowへカーソルを移す
(global-set-key (kbd "C-S-o") 'other-window-or-split)

;; 画面左にディレクトリツリーを表示
(require 'neotree)
(defun my-neotree-toggle ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (subwindow-hide)
    (imenu-list-minor-mode -1) ;; neotreeを表示するときなimenuをかくす
    ;; (subwindow-hide)
    (neotree-show)))

(global-set-key (kbd "M-S") 'subwindow-toggle)
(global-set-key (kbd "M-N") 'my-neotree-toggle)
(add-hook
 'neotree-mode-hook
 (lambda ()
   (local-set-key (kbd "j") 'neotree-next-line)
   (local-set-key (kbd "k") 'neotree-previous-line)
   (local-set-key (kbd "u") 'neotree-select-up-node)
   (local-set-key (kbd "C-j") 'neotree-quick-look)
   ))
(setq neo-smart-open t)

;; imenu
(with-eval-after-load "imenu-list"
  (define-key imenu-list-major-mode-map (kbd "j") 'next-line)
  (define-key imenu-list-major-mode-map (kbd "k") 'previous-line)
  (define-key imenu-list-major-mode-map (kbd "C-j") 'imenu-list-display-entry)
  )

(custom-set-variables '(imenu-list-size 0.2))

(defun my-imenu-list-smart-toggle ()
  (interactive)
  (if (get-buffer-window imenu-list-buffer-name t)
      (imenu-list-minor-mode -1)
    (neotree-hide) ;; imenuを表示するときはneotreeをかくす
    (subwindow-hide)
    (imenu-list-minor-mode 1)
    (other-window 1)
    ))

(global-set-key (kbd "M-I") 'my-imenu-list-smart-toggle)

;; (global-set-key "\M-g" 'goto-line)
;; (global-set-key (kbd "C-M-g") 'igrep)
(global-set-key (kbd "C-x g") 'grep-find)
(global-set-key (kbd "C-M-e") 'ediff-merge-files)
;; (global-set-key "\M-o" 'occur-by-moccur)
;; (global-set-key (kbd "C-M-o") 'moccur)
(global-set-key (kbd "C-x C-o") 'moccur-grep-find)
;; (global-set-key [\C-\tab] 'dabbrev-expand)
(global-set-key (kbd "C-c C-i") 'indent-region)
(global-set-key (kbd "C->") 'comment-region)
(global-set-key (kbd "C-<") 'uncomment-region)
;; 改行と同時にインデント
(global-set-key (kbd "C-m") 'newline-and-indent)
;; C-h でバックスペース
(keyboard-translate ?\C-h ?\C-?) ; C-h -> BS
(global-set-key (kbd "C-H") 'help-command)

;; ;; 文字の拡大、縮小、元に戻す
;; (global-set-key (kbd "C-M-;") (lambda () (interactive) (text-scale-increase 1)))
;; ;; 文字の縮小
;; (global-set-key (kbd "C-M--") (lambda () (interactive) (text-scale-decrease 1)))
;; 文字のサイズを元に戻す
(global-set-key (kbd "C-M-0") (lambda () (interactive) (text-scale-increase 0)))

;; http://www.gentei.org/~yuuji/software/euc/instamp.el
;; 現在時刻挿入
(autoload 'instamp
  "instamp" "Insert TimeStamp on the point" t)
(define-key global-map "\M-s" 'instamp)
(setq instamp-date-format-list-private
      '("%Y%m%d"))

;; 行末の空白を削除する
(global-set-key (kbd "C-x C-s") 'delete-trailing-whitespace)

;; skk
(global-set-key "\C-\\" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(global-set-key "\C-xt" 'skk-tutorial)
(global-set-key (kbd "C-t") 'skk-latin-toggle)

;; 5.8 goto-chg.el
;; 最後の変更箇所にジャンプする
(require 'goto-chg)
(define-key global-map (kbd "<f8>") 'goto-last-change)
(define-key global-map (kbd "S-<f8>") 'goto-last-change-reverse)

;; undo
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (global-set-key (kbd "C-M-/") 'undo-tree-redo))

;;; windmove (e2wm でバッファ切り換えがしやすくなるように)
(windmove-default-keybindings)
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-left>"))

(global-set-key (kbd "<C-up>") (quote windmove-up))
(global-set-key (kbd "<C-down>") (quote windmove-down))
(global-set-key (kbd "<C-right>") (quote windmove-right))
(global-set-key (kbd "<C-left>") (quote windmove-left))

