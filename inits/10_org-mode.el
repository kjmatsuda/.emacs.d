;;;;;;;;;;;;;;;;;  Emacsテクニックバイブルより ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14. org-mode
(require 'org)

;; 14.4 M-x org-remember
(setq org-directory "~/Dropbox/org/")
(setq org-mobile-directory "~/Dropbox/mobileorg/")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/goals.org")

(setq org-default-notes-file (expand-file-name "capture/capture.org" org-directory))
(require 'org-capture)
(setq org-capture-templates
      '(("t" "Task" entry (file+headline nil "Inbox")
         "** TODO %?\n %T\n %a\n %i\n")
        ("b" "Bug" entry (file+headline nil "Inbox")
         "** TODO %?   :bug:\n  %T\n %a\n %i\n")
        ("i" "Idea" entry (file+headline nil "Idea")
         "** %?\n %U\n %i\n %a\n %i\n")
        ("c" "Comment" entry (file+headline nil "Code Reading")
         "** %?\n %U\n %i\n %a\n %i\n")
        ("w" "Twitter" entry (file+datetree "twitter.org")
         "** %U %?\n")
        )
      )
(global-set-key (kbd "C-,") 'org-capture)

;; 14.6 TODOリストを作成する
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCEL(c)")))

;; 14.14 予定表を見る
;; アジェンダ表示の対象ファイル
(setq org-agenda-files (list org-directory))

(global-set-key (kbd "C-c a") 'org-agenda)

;; mobileorg のアジェンダ表示を改善するべく、org-agenda-custom-commands を触った。2015/3/26
;; どこかが悪さをして、orgmobile から google カレンダーへの同期が効かなくなったので
;; いくつかコメントアウトした。
;; どれがまずかったのかまでは分からない。org-agenda-sorting-strategy,org-deadline-warning-days
(setq org-agenda-custom-commands
      '(
        ;; ("d" "全般：今日すること"
        ;;  ((agenda "" ((org-agenda-ndays 1)
        ;;               (org-deadline-warning-days 1) ;; 締切日の一日前のアジェンダに表示
        ;;               ;; (org-agenda-sorting-strategy
        ;;               ;;  (quote ((agenda time-up priority-down tag-up))))
        ;;               ;; (org-deadline-warning-days 0)
        ;;               ))))
        ;; ("w" "全般：今週すること"
        ;;  ((agenda "" ((org-agenda-ndays 7)
        ;;               (org-deadline-warning-days 7)
        ;;               ;; (org-agenda-sorting-strategy
        ;;               ;;  (quote ((agenda time-up priority-down tag-up))))
        ;;               ;; (org-deadline-warning-days 0)
        ;;               ))))
        ;; ("g" "目標"
        ;;  ((tags-todo "goals")))
        ("h" "目標：今日すること" ((agenda "" ((org-agenda-ndays 1)
                                               (org-deadline-warning-days 1)
                                                ;; (org-agenda-sorting-strategy
                                                ;;  (quote ((agenda time-up priority-down tag-up))))
                                                ;; (org-deadline-warning-days 0)
                                                )))
         ((org-agenda-files '("~/Dropbox/org/goals.org" "~/Dropbox/org/projects.org"))
          ;; (org-agenda-sorting-strategy '(priority-up effort-down))
          )
         ;; ("~/computer.html")
         )
        ("j" "目標：今週すること" ((agenda "" ((org-agenda-ndays 7)
                                               (org-deadline-warning-days 7)
                                                ;; (org-agenda-sorting-strategy
                                                ;;  (quote ((agenda time-up priority-down tag-up))))
                                                ;; (org-deadline-warning-days 0)
                                                )))
         ((org-agenda-files '("~/Dropbox/org/goals.org" "~/Dropbox/org/projects.org"))
          ;; (org-agenda-sorting-strategy '(priority-up effort-down))
          )
         ;; ("~/computer.html")
         )
        ))

;; Googleカレンダーへエスクポート
;; ネタ元
;; http://d.hatena.ne.jp/t0m0_tomo/20100103/1262537012
(setq org-combined-agenda-icalendar-file "~/Dropbox/calendar/org.ics")
(setq org-icalendar-include-todo t)
;; (setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))
;; (setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(event-if-todo))
(setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))

;; ;; org ファイルを google カレンダーに同期する
;; (defun start-process-org ()
;;   (interactive)
;;   (start-process-shell-command "org-sync-gcal" "*org-sync-gcal*" "emacs" "--script" "~/Dropbox/.emacs.d/elisp/org-sync-gcal.el"))
;; (define-key global-map [f12] 'start-process-org)


(if (linux?)
    (progn
      ;; (start-process-org)
      ;; Emacs 終了時に mobileorg に push
      ;; (add-hook 'kill-emacs-hook 'org-mobile-push)
      (add-hook 'kill-emacs-hook
                '(lambda nil
                   (shell-command "set_goals_score")))))
;; サブタスクが残っているときに親タスクをDONEにできないようにする
(setq org-enforce-todo-dependencies t)

;; サブタスクが全て DONE になったら親タスクも自動的に DONE になり
;; サブタスクをひとつでも TODO にしたら 親タスクも TODO になる
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; カーソル位置のステータスを取得する関数
;; 返り値 :item, :headline, :headline-stars
(defun org-position-status(context-list)
  (if (not (cdr context-list))
      ;; context-list の末尾に到達
      (caar context-list)
    (org-position-status (cdr context-list))))

(add-hook
 'org-mode-hook
 (lambda ()
   (local-set-key (kbd "<M-S-return>") (lambda () (interactive)
                                       (if (not (eq (org-position-status (org-context)) :item))
                                           (progn
                                             (org-insert-heading-after-current))
                                         (org-insert-heading))))
   (local-set-key (kbd "<M-return>") (lambda () (interactive)
                                         (org-insert-todo-heading-respect-content)
                                         (insert (format "%s" "[0/1] "))))
   ;; 自作の M-return の動きに干渉するので key-combo-mode をオフにする
   (key-combo-mode -1)
   (auto-complete-mode t)
   ))

;; elscreen で<C-tab>をタブの切り換えに割り当てたいので無効にする
(define-key org-mode-map [(control tab)] nil)
(define-key org-mode-map (kbd "<M-S-return>") (lambda () (interactive)
                                       (if (not (eq (org-position-status (org-context)) :item))
                                           (progn
                                             (org-insert-heading-after-current))
                                         (org-insert-heading))))
(define-key org-mode-map (kbd "<M-return>") (lambda () (interactive)
                                         (org-insert-todo-heading-respect-content)
                                         (insert (format "%s" "[0/1] "))))

;; org-babel
(org-babel-do-load-languages    ;;; org7.5/doc/org.pdf p162
  'org-babel-load-languages
  '((R . t)
    (sh . t)
    (C . t)
    (dot . t)))

;; org ファイル読み込み時に自動的に画像をインライン表示する
;; 読み込み後、編集中の画像リンクには影響しない
(setq org-startup-with-inline-images t)


;; 常に画像を表示
;; リンク記述後 C-l で即表示
(add-hook 'org-mode-hook 'turn-on-iimage-mode)

;;;;;;;;;;;;;;;; mobile-orgとの同期 START ;;;;;;;;;;;;;;;;;;;;
;;;;; 参考 http://tokikane-tec.blogspot.jp/2015/01/org-mobile-pullpush_21.html
;; (require 'org-mobile)
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
;; (require 'org-mobile-sync)
;; (org-mobile-sync-mode 1)