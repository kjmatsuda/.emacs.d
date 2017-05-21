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

