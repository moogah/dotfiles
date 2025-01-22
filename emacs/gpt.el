(use-package shell-maker
  :straight (shell-maker :type git :host github :repo "xenodium/shell-maker" :files ("shell-maker.el")))

; add :build (:not compile) if byte code has problems
(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("*.el") :build (:not compile)))

(setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com")))

;;(setq chatgpt-shell-model-version "gpt-4o")

(use-package gptel
  :straight t
  :custom
  (gptel-model "gpt-4o"))

(use-package elysium
  :straight (:host github :repo "lanceberge/elysium" :branch "main" :files ("*.el")))
