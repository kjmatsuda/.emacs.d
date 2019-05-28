(if (string= system-name "KHW-CP0075")
    (progn
      ;; 日本語フォント(社用)
      (cond
       (window-system
        (set-default-font "Ricty Diminished-12")
        )
       )

      (setq org-agenda-files (list "~/WORK/org/"))

      ;; ローカルに ical をエクスポート
      ;; http://d.hatena.ne.jp/t0m0_tomo/20100103/1262537012
      (setq org-combined-agenda-icalendar-file "~/WORK/calendar/org.ics")

      (defun org-push-daily-my ()
        (interactive)
        (start-process "org-to-ics" "*org-to-ics*" "emacs" "--script" "~/org-to-ics.el"))
      (define-key global-map [f12] 'org-push-daily-my)

      ;; org ファイルを ics に変換
      (org-push-daily-my)
      ))
