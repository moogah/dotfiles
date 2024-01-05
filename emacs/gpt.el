(use-package shell-maker
  :straight (shell-maker :type git :host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :straight (chatgpt-shell :type git :host github :repo "xenodium/chatgpt-shell" ))

(setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com")))
