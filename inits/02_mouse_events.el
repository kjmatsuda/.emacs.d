(use-package right-click-context)

(custom-set-variables
 '(right-click-context-global-menu-tree
   '(("Tags"
      ("Jump"  :call (my/helm-gtags-find-tag-from-here))
      ("Reference" :call (helm-gtags-find-rtag))
      ("Pop" :call (helm-gtags-pop-stack))
      )
     ("View"
      ("Tree"  :call (neotree-toggle))
      ("imenu"  :call (imenu-list-smart-toggle)) ;; 関数リストの表示のトグ
      )
     ("File"
      ("Open"  :call nil) ;; TODO ファイル選択
      )
     ("Buffer"
      ("Select"  :call nil) ;; TODO バッファ選択
      )
     ("Quit" :call nil)
     )))

(right-click-context-mode 1)

;; スクロールバーのドラッグが効くようにする
(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)
(global-set-key [vertical-scroll-bar drag-mouse-1] 'scroll-bar-drag)
(global-set-key [vertical-scroll-bar vertical-scroll-bar mouse-1] 'scroll-bar-drag)

;; ダブルクリックで定義ジャンプ
(global-set-key [double-mouse-1] 'my/helm-gtags-find-tag-from-here)

;; 不要な領域選択を抑止する
(global-unset-key [drag-mouse-1])
;; (global-unset-key [down-mouse-1])

;;;;;;;;;;;;;;; neotree ;;;;;;;;;;;;;;; 
;; neotree上でクリックでファイルを開く
(defun click-neotree-select-file (click)
  (interactive "e")
  (mouse-set-point click nil)
  (neotree-enter))

(with-eval-after-load 'neotree
  (define-key neotree-mode-map [mouse-1] 'click-neotree-select-file))

;; (..)をクリックするとカレントディレクトリの1つ上の階層を表示する
(custom-set-variables '(neo-click-changes-root t))

;;;;;;;;;;;;;; imenu ;;;;;;;;;;;;;;;;;; 
;; imenu-list上でクリックで定義に移動
(defun click-imenu-list-goto-entry (click)
  (interactive "e")
  (mouse-set-point click nil)
  (imenu-list-goto-entry))

(with-eval-after-load 'imenu-list
  (define-key imenu-list-major-mode-map [mouse-1] 'click-imenu-list-goto-entry))
