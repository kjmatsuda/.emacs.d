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

;; clojure開発用にウィンドウを構成する
(defun window-manage-for-develop ()
  (interactive)
  (delete-other-windows)
  (neotree)
  (other-window 1)
  (split-window-vertically (/ (* 2 (window-height)) 3))
  (other-window 1)
  (if (eq major-mode 'clojure-mode)
      (select-buffer-start-with (buffer-list) "*cider-repl")
    (if (eq major-mode 'lisp-mode)
        (select-buffer-start-with (buffer-list) "*slime-repl")))
  (other-window 1)
  (other-window 1))

;; C-S-oで次のwindowへカーソルを移す
(global-set-key (kbd "C-S-o") 'other-window-or-split)

;; 画面左にディレクトリツリーを表示
(require 'neotree)
(global-set-key (kbd "M-+") 'window-manage-for-develop)
(global-set-key (kbd "M--") 'neotree-hide)

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
