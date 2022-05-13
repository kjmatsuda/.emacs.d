;; 2022/05/13(金)
;; ripgrep をインストールした際に Windows では Emacs から ripgrep を起動したときに、日本語の検索がうまくいかなかった。
;; そのための対策を以下のネット記事から拾ってきた。
;; UTF-8 をベースとして利用するための設定 - NTEmacs @ ウィキ - atwiki（アットウィキ）
;; https://w.atwiki.jp/ntemacs/pages/16.html
(if (win?)
    (progn
      (require 'cl-lib)

      (setenv "LANG" "ja_JP.UTF-8")

      ;; IME の設定をした後には実行しないこと
      ;; (set-language-environment "Japanese")

      (prefer-coding-system 'utf-8-unix)
      (set-file-name-coding-system 'cp932)
      (setq locale-coding-system 'utf-8-unix)

      ;; プロセスが出力する文字コードを判定して、process-coding-system の DECODING の設定値を決定する
      (setq default-process-coding-system '(undecided-dos . utf-8-unix))

      ;; サブプロセスに渡すパラメータの文字コードを cp932 にする
      (cl-loop for (func args-pos) in '((call-process        4)
                                        (call-process-region 6)
                                        (start-process       3))
               do (eval `(advice-add ',func
                                     :around (lambda (orig-fun &rest args)
                                               (setf (nthcdr ,args-pos args)
                                                     (mapcar (lambda (arg)
                                                               (if (multibyte-string-p arg)
                                                                   (encode-coding-string arg 'cp932)
                                                                 arg))
                                                             (nthcdr ,args-pos args)))
                                               (apply orig-fun args))
                                     '((depth . 99)))))))

