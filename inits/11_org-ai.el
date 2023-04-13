;; rksm/org-ai: Emacs as your personal AI assistant using ChatGPT, DALL-E, Whisper
;; https://github.com/rksm/org-ai#setting-up-speech-input--output

(straight-use-package
 '(org-ai :type git :host github :repo "rksm/org-ai"
          :local-repo "org-ai"
          :files ("*.el" "README.md" "snippets")))

(use-package org-ai
  :ensure
  :commands (org-ai-mode)
  :custom
  (org-ai-openai-api-token (getenv "OPENAI_API_KEY"))
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  :config
  ;; if you are on the gpt-4 beta:
  ;; (setq org-ai-default-chat-model "gpt-4")
  ;; if you are using yasnippet and want `ai` snippets
  (org-ai-install-yasnippets)
  )

;; Setting up speech input / output
(straight-use-package
 '(whisper.el :type git :host github :repo "natrys/whisper.el"
          :local-repo "whisper.el"
          :files ("*.el" "README.org")
          :bind ("C-M-w" . whisper-run)))

(use-package greader :ensure)
(require 'whisper)
(require 'org-ai-talk)
