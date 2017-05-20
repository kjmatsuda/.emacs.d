;;;; デスクトップの conky に表示するためにこの el ファイルを使う
;;;; 以下の要領で起動 
;;;; emacs -batch -l /home/koji/.emacs.d/init_org_agenda.el -eval '(org-batch-agenda "a")'

;;; 14. org-mode
(require 'org)

;; 14.6 TODOリストを作成する
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCEL(c)")))

;; 14.14 予定表を見る
;; アジェンダ表示の対象ファイル
(setq org-agenda-files '("~/Dropbox/org/goals.org"
                         "~/Dropbox/org/projects.org"))
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-custom-commands
      '(("a" "目標：今月すること" ((agenda "" ((org-agenda-ndays 30)
                                               (org-deadline-warning-days 7) ;; 締切日の一週間前のアジェンダに表示
                                               ;; (org-agenda-sorting-strategy
                                               ;;  (quote ((agenda time-up priority-down tag-up))))
                                               ;; (org-deadline-warning-days 0)
                                               )))
         ;; (
         ;;  (org-agenda-files '("~/Dropbox/org/goals.org"))
         ;;  ;; (org-agenda-sorting-strategy '(priority-up effort-down))
         ;;  )
         )
        ))

