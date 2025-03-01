;;;;;;;;;;;;;;;;;  Emacsテクニックバイブルより ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14. org-mode
(use-package org)
(use-package org-agenda)

;; 14.4 M-x org-remember
(setq org-directory "~/work/org/")
(setq org-mobile-directory "~/work/mobileorg/")
(setq org-mobile-inbox-for-pull "~/work/org/goals.org")

(setq org-default-notes-file (expand-file-name "capture.org" org-directory))
(use-package org-capture)
(setq org-capture-templates
      '(("e" "Embedded" entry (file+headline org-default-notes-file "Embedded")
         "** TODO %?\n %U\n %a\n %i\n")
        ("c" "C,C++" entry (file+headline org-default-notes-file "C,C++")
         "** TODO %?\n %U\n %a\n %i\n")
        ("w" "Web" entry (file+headline org-default-notes-file "Web")
         "** TODO %?\n %U\n %a\n %i\n")
        ("j" "JavaScript" entry (file+headline org-default-notes-file "JavaScript")
         "** TODO %?\n %U\n %a\n %i\n")
        ("n" "Node.js" entry (file+headline org-default-notes-file "Node.js")
         "** TODO %?\n %U\n %a\n %i\n")
        ("r" "React.js" entry (file+headline org-default-notes-file "React.js")
         "** TODO %?\n %U\n %a\n %i\n")
        ("d" "Data Analysis" entry (file+headline org-default-notes-file "Data Analysis")
         "** TODO %?\n %U\n %a\n %i\n")
        ("a" "Asset Management" entry (file+headline org-default-notes-file "Asset Management")
         "** TODO %?\n %U\n %a\n %i\n")
        ("f" "Finance" entry (file+headline org-default-notes-file "Finance")
         "** TODO %?\n %U\n %a\n %i\n")
        ("s" "Software Architecture" entry (file+headline org-default-notes-file "Software Architecture")
         "** TODO %?\n %U\n %a\n %i\n")
        ("b" "Building Architecture" entry (file+headline org-default-notes-file "Building Architecture")
         "** TODO %?\n %U\n %a\n %i\n")
        ("p" "Politics" entry (file+headline org-default-notes-file "Politics")
         "** TODO %?\n %U\n %a\n %i\n")
        ("i" "Idea" entry (file+headline org-default-notes-file "Idea")
         "** TODO %?\n %U\n %a\n %i\n")
        ("o" "Others" entry (file+headline org-default-notes-file "Others")
         "** TODO %?\n %U\n %a\n %i\n")
        )
      )
(global-set-key (kbd "C-;") 'org-capture)

(setq org-log-done 'time)

(setq org-agenda-log-mode-items '(closed clock))

;; 14.6 TODOリストを作成する
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCEL(c)")))

;; 14.14 予定表を見る
;; アジェンダ表示の対象ファイル
(setq org-agenda-files (list org-directory))

(global-set-key (kbd "C-c a") 'org-agenda-cache)

;; mobileorg のアジェンダ表示を改善するべく、org-agenda-custom-commands を触った。2015/3/26
;; どこかが悪さをして、orgmobile から google カレンダーへの同期が効かなくなったので
;; いくつかコメントアウトした。
;; どれがまずかったのかまでは分からない。org-agenda-sorting-strategy,org-deadline-warning-days
(setq org-agenda-custom-commands
      '(
        ("d" "今日の予定" (
                           (agenda "" ((org-agenda-span 1)
                                       (org-deadline-warning-days 1)
                                       ;; (org-agenda-show-log nil)
                                       (org-agenda-clockreport-mode nil)
                                       ))
                           )
         (
          ;; (org-agenda-files '("~/work/org/goals.org" "~/work/org/projects.org" "~/work/org/inbox.org" "~/work/org/dev_env.org"))
          (org-agenda-sorting-strategy '(time-up priority-down effort-down))
          )
         ;; ("~/computer.html")
         )
        ("l" "今日の作業記録" (
                           (agenda "" ((org-agenda-span 1)
                                       (org-agenda-show-log 'clockcheck)
                                       (org-agenda-clockreport-mode t)))
                           )
         (
          ;; (org-agenda-files '("~/work/org/goals.org" "~/work/org/projects.org" "~/work/org/inbox.org" "~/work/org/dev_env.org"))
          (org-agenda-sorting-strategy '(time-up priority-down effort-down))
          )
         )
        ("w" "今週の予定" (
                           (agenda "" ((org-agenda-span 7)
                                       (org-deadline-warning-days 7)
                                       (org-agenda-clockreport-mode nil)
                                       ))
                           )
         (
          ;; (org-agenda-files '("~/work/org/goals.org" "~/work/org/projects.org" "~/work/org/inbox.org" "~/work/org/dev_env.org"))
          (org-agenda-sorting-strategy '(time-up priority-down effort-down))
          )
         )
        ("m" "今月の予定" (
                           (agenda "" ((org-agenda-span 'month)
                                       (org-deadline-warning-days 7)
                                       (org-agenda-clockreport-mode nil)
                                       ))
                           )
         (
          ;; (org-agenda-files '("~/work/org/goals.org" "~/work/org/projects.org" "~/work/org/inbox.org" "~/work/org/dev_env.org"))
          (org-agenda-sorting-strategy '(time-up priority-down effort-down))
          )
         )
        ))

;;;; org-agendaの日付表示を日本語にする
(defadvice org-agenda (around org-agenda-around)
  (let ((system-time-locale "Japanese"))
    ad-do-it))

(defadvice org-agenda-redo (around org-agenda-redo-around)
  (let ((system-time-locale "Japanese"))
    ad-do-it))

(custom-set-variables
  '(org-agenda-format-date "%Y/%m/%d (%a)"))

(custom-set-faces
 '(org-agenda-date ((t :weight bold))))

;;;; 【もう遅くない】Emacs org-agendaによる予定表を瞬時に表示させる方法
;; http://emacs.rubikitch.com/org-agenda-cache/
(defun org-agenda-cache (&optional regenerate)
  "agendaを更新せずに表示する。"
  (interactive "P")
  (if (or regenerate (null (get-buffer "*Org Agenda*")))
    (progn
      (setq current-prefix-arg nil)
      (org-agenda nil))
  (delete-other-windows)
  (split-window-horizontally (/ (window-width) 2))
  (other-window 1)
  (switch-to-buffer "*Org Agenda*"))
  )
;;; そもそもqで*Org Agenda*をkillしないようにする
(define-key org-agenda-mode-map (kbd "q") 'delete-window)

;;;; 子タスクをもつタスクがグレーアウト表示されないようにする
(setq org-agenda-dim-blocked-tasks nil)

;; Googleカレンダーへエスクポート
;; ネタ元
;; http://d.hatena.ne.jp/t0m0_tomo/20100103/1262537012
(setq org-combined-agenda-icalendar-file "~/work/calendar/org.ics")
(setq org-icalendar-include-todo t)
;; (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))
;; (setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(event-if-todo))
(setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))

;; (if (linux?)
;;     (progn
;;       ;; (start-process-org)
;;       ;; Emacs 終了時に mobileorg に push
;;       ;; (add-hook 'kill-emacs-hook 'org-mobile-push)
;;       (add-hook 'kill-emacs-hook
;;                 '(lambda nil
;;                    (shell-command "set_goals_score")))))
;; サブタスクが残っているときに親タスクをDONEにできないようにする
(setq org-enforce-todo-dependencies t)

;; サブタスクが全て DONE になったら親タスクも自動的に DONE になり
;; サブタスクをひとつでも TODO にしたら 親タスクも TODO になる
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (org-todo (if (= n-not-done 0) "DONE" "TODO")))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; カーソル位置のステータスを取得する関数
;; 返り値 :item, :headline, :headline-stars
(defun org-position-status(context-list)
  (if (not (cdr context-list))
      ;; context-list の末尾に到達
      (caar context-list)
    (org-position-status (cdr context-list))))

(defun org-set-image-width (width)
  "set org image width"
  (interactive "nInput image width(0~) :")
  (move-beginning-of-line nil)
  (if (eq (org-position-status (org-context)) :link)
      ;; リンク上にカーソルがある場合
      (progn
        ;; リンクの1行上にカーソル移動
        (previous-line)
        (if (not (eq nil (org-position-status (org-context))))
            (progn (end-of-line)
                   (newline))))
    (if (not (eq nil (org-position-status (org-context))))
        (progn (end-of-line)
               (newline))))
  (move-beginning-of-line nil)
  (if (string-match "+ATTR_ORG" (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
      (kill-line))
  ;; プロンプト表示して、widthをユーザに入力させる
  (insert-string (concat "#+ATTR_ORG: :width " (number-to-string width)))
  (org-redisplay-inline-images))

;; Emacs lisp の評価結果をプリント
(defun my-eval-print-last-sexp (&optional eval-last-sexp-arg-internal)
  (interactive "P")
  (let ((eval-result (eval-last-sexp nil))
        (standard-output (current-buffer)))
    (insert (format " => %s" eval-result))))

(add-hook
 'org-mode-hook
 (lambda ()
   (local-set-key (kbd "<M-return>") (lambda () (interactive)
                                       (if (not (eq (org-position-status (org-context)) :item))
                                           (progn
                                             (org-insert-todo-heading-respect-content)
                                             (insert (format "%s" "[0/1] ")))
                                         (org-insert-item))))
   (local-set-key (kbd "C-c C-x C-r") 'org-clock-report)
   ;; 議事録などを書くときによく使う文字列を登録
   (local-set-key (kbd "C-<") (lambda () (interactive) (insert-string " <- ")))
   (local-set-key (kbd "C->") (lambda () (interactive) (insert-string " -> ")))
   ;; orgのリンクをコピー、ペーストするキーバインドを登録
   (local-set-key (kbd "C-c C-w") 'org-store-link)
   (local-set-key (kbd "C-c C-y") 'org-insert-last-stored-link)
   (local-set-key (kbd "C-c C-M-y") 'org-insert-all-links)
   (local-set-key (kbd "C-o") 'org-mark-ring-goto)
   (local-set-key (kbd "C-]") 'org-open-at-point)
   ;; 画像関連
   (local-set-key (kbd "C-c u") 'org-redisplay-inline-images)
   (local-set-key (kbd "C-c s") 'org-set-image-width)
   (local-set-key (kbd "C-x C-e") 'my-eval-print-last-sexp)

   (global-set-key (kbd "C-c p") 'my-org-screenshot)
   ))

;; orgでリンクを開く際に同一フレームで開くようにする
(setq org-link-frame-setup (quote ((vm . vm-visit-folder-other-frame)
               (vm-imap . vm-visit-imap-folder-other-frame)
               (gnus . org-gnus-no-new-news)
               (file . find-file)
               (wl . wl-other-frame))))

;; elscreen で<C-tab>をタブの切り換えに割り当てたいので無効にする
(define-key org-mode-map [(control tab)] nil)


;; org-eldoc
(use-package org-eldoc
  :config
  (defadvice org-eldoc-documentation-function (around add-field-info activate)
    (or
     (ignore-errors (and (not (org-at-table-hline-p)) (org-table-field-info nil)))
     ad-do-it))

  (add-hook 'org-mode-hook 'eldoc-mode)

  (eldoc-add-command-completions
   "org-table-next-" "org-table-previous" "org-cycle")

  ;; org-babel
  (org-babel-do-load-languages    ;;; org7.5/doc/org.pdf p162
   'org-babel-load-languages
   '((R . t)
     (sh . t)
     (C . t)
     (dot . t)))
)

;; org ファイル読み込み時に自動的に画像をインライン表示する
;; 読み込み後、編集中の画像リンクには影響しない
(setq org-startup-with-inline-images t)
;; 画像サイズを設定できるようにする
(setq org-image-actual-width nil)

;; 常に画像を表示
;; リンク記述後 C-l で即表示
(add-hook 'org-mode-hook 'turn-on-iimage-mode)

;;;;;;;; org-modeでプレゼンテーション ;;;;;;;;
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-display-inline-images)
                 (org-present-read-only)
                 (org-present-hide-cursor)
                 ))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-read-write)
                 (org-present-show-cursor)
                 ))
     ;; 文字をどれだけ大きくするかを設定する
     (setq org-present-text-scale 7)
     ;; 日本語キーボードの人はC-c C-;に割り当てるとよい
     (define-key org-present-mode-keymap (kbd "C-c C-;") 'org-present-big)
     (define-key org-present-mode-keymap (kbd "j") 'org-present-next)
     (define-key org-present-mode-keymap (kbd "k") 'org-present-prev)
     (define-key org-present-mode-keymap (kbd "M->") 'org-present-end)
     (define-key org-present-mode-keymap (kbd "M-<") 'org-present-beginning)
     (define-key org-present-mode-keymap (kbd "q") 'org-present-quit)
     ))

;;;;;;;; org-modeに画像をD&Dでダウンロード(org-download) ;;;;;;;;
(use-package org-download
  :config
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq-default org-download-image-dir "~/work/org/org-download")
  )

;; org 9.4 くらいから、初期表示時にヘッダが展開した状態になっているので、初期表示時は閉じた状態にする
(setq org-startup-folded t)

;; disable indentation
(setq org-adapt-indentation nil)

;;;;;;;;  org-mode にスクリーンショットを貼り付けられるようにするための設定 ;;;;;;;;
(if (win?)
    ;; emacs org-mode用の画像貼り付るための設定 - i123のブログ
    ;; https://i123.hatenablog.com/entry/2022/06/12/001529
    (progn
      (defun my-org-screenshot ()
        "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
        (interactive)

        (setq temp-filename
              (concat
               (make-temp-name
                (concat "images\\"
                        (file-name-sans-extension (buffer-name))
                        "_"
                        (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))

        ;; (shell-command "snippingtool /clip") ;; 有効にするとツールで範囲指定した箇所を取り込みできる
        (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('"temp-filename"',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
        (setq org-image-filename (replace-regexp-in-string "\\\\" "\/" temp-filename t t))
        (insert (concat "[[file:" org-image-filename "]]"))
        (org-display-inline-images))
      )
  )

(if (linux?)
    ;; clipboard - How to paste all images into emacs org-mode running in WSL in one directory - Emacs Stack Exchange
    ;; https://emacs.stackexchange.com/questions/74045/how-to-paste-all-images-into-emacs-org-mode-running-in-wsl-in-one-directory
    (progn
      (defun my-org-screenshot (image-dir image-name)
        "Paste an image into a time stamped unique-named file in the
 same directory as the org-buffer and insert a link to this file."
        (interactive "DImage Directory: \nsStart Image Name with: ")
        (let* ((target-file
                ;; This creates an empty file
                (let ((temporary-file-directory image-dir))
                  (make-temp-file (format "%s%s_%s"
                                          (file-name-as-directory image-dir)
                                          image-name
                                          (format-time-string "%Y%m%d_%H%M%S_"))
                                  nil ".png")))
               (wsl-path
                (concat (as-windows-path (file-name-directory target-file))
                        "\\"
                        (file-name-nondirectory target-file)))
               (ps-script
                (concat "(Get-Clipboard -Format image).Save('" wsl-path "')")))
          ;; Delete the empty file
          (message "Deleting empty file %s" target-file)
          (delete-file target-file t)
          (powershell ps-script)
          (if (file-exists-p target-file)
              (progn (insert (concat "[[" "file:" (expand-file-name target-file) "]]"))
                     (org-display-inline-images))
            (user-error
             "Error pasting the image, make sure you have an image in the clipboard!"))))

      (defun as-windows-path (unix-path)
        "Takes a unix path and returns a matching WSL path."
        ;; (e.g. \\wsl$\Ubuntu-20.04\tmp)
        ;; substring removes the trailing \n
        (substring
         (shell-command-to-string
          (concat "wslpath -w " unix-path))
         0 -1))

      (defun powershell (script)
        "executes the given script within a powershell and returns its return value"
        (call-process "powershell.exe" nil nil nil
                      "-Command" (concat "& {" script "}")))
  )
)

;;;;;;;;;;;;;;;; mobile-orgとの同期 START ;;;;;;;;;;;;;;;;;;;;
;;;;; 参考 http://tokikane-tec.blogspot.jp/2015/01/org-mobile-pullpush_21.html
;; (use-package org-mobile)
;; (defvar org-mobile-sync-timer nil
;;   "Timer that `org-mobile-sync-timer' used to reschedule itself, or nil.")

;; ;;pullしてpush する関数
;; (defun org-mobile-sync nil
;;   (interactive)
;;   (org-mobile-pull)
;;   (org-mobile-push))

;; ;;idle 時間が30秒経過すると自動でpullしてpush
;; (defun org-mobile-set-sync-timer nil
;;   (interactive)
;;   (setq org-mobile-sync-timer (run-with-idle-timer 10 30 t 'org-mobile-sync)))

;; ;;自動pull, push  を無効にする
;; (defun org-mobile-clear-sync-timer nil
;;   (interactive)
;;   (cancel-timer org-mobile-sync-timer))

;; ;;指定されたファイルを指定された時間で監視，更新されたらorg-mobile-pull を実行
;; (defun install-monitor (file secs)
;;   (run-with-timer
;;    0 secs
;;    (lambda (f p)
;;      (unless (< p (second (time-since (elt (file-attributes f) 5))))
;;        (org-mobile-pull)
;;        ;; (message "mobileorg.org is updated!")
;;        ))
;;    file secs))

;; ;; capture ファイルを10秒ごとに監視，更新されたらorg-mobile-pull 実行
;; (install-monitor (file-truename
;;                   (concat
;;                    (file-name-as-directory org-mobile-directory)
;;                    org-mobile-capture-file))
;;                  10)

;; ;; org-mobile に登録したファイルを保存するとpush
;; (add-hook 'after-save-hook
;;           (lambda ()
;;             (when (eq major-mode 'org-mode)
;;               (dolist (file (org-mobile-files-alist))
;;                 (if (string= (file-truename (expand-file-name (car file)))
;;                              (file-truename (buffer-file-name)))
;;                     (org-mobile-push)))
;;               )))

;; ;; org-mobile に登録したファイルを開くとpull
;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (when (eq major-mode 'org-mode)
;;               (dolist (file (org-mobile-files-alist))
;;                 (if (string= (file-truename (expand-file-name (car file)))
;;                              (file-truename (buffer-file-name)))
;;                     (org-mobile-pull)))
;;               )))

;; (org-mobile-set-sync-timer)
;;;;;;;;;;;;;;;;;;; mobile-orgとの同期 END ;;;;;;;;;;;;;;;;;;;;;;;

;; どうもこれが有効になってるとwindows 10 で時折固まるので、コメントアウトする 2017/5/19(金)
;; (use-package org-mobile-sync)
;; (org-mobile-sync-mode 1)
