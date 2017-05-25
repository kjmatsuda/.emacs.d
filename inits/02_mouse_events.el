(require 'right-click-context)

(custom-set-variables
 '(right-click-context-global-menu-tree
   '(("Tags"
      ("Jump"  :call (my/helm-gtags-find-tag-from-here))
      ("Reference" :call (helm-gtags-find-rtag))
      ("Pop" :call (helm-gtags-pop-stack))
      )
     ("View"
      ("Tree"  :call (neotree-toggle))
      ("imenu"  :call nil) ;; TODO 関数リストの表示のトグ
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

;; TODO ダブルクリックでQuit



