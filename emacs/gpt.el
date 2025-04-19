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
  (gptel-model 'gpt-4o) ;; model is now a symbol, not a string
  :config
  ;; Configure Perplexity backend
  (gptel-make-perplexity "Perplexity"
    :key (lambda () (auth-source-pick-first-password :host "api.perplexity.ai"))
    :stream t)

  ;; Configure Anthropic backend for Claude Sonnet
  (gptel-make-anthropic "Claude"
    :stream t
    :key (lambda () (auth-source-pick-first-password :host "api.anthropic.com")))

  ;; Configure Anthropic backend for Claude 3.7 Sonnet with thinking mode
  (gptel-make-anthropic "Claude-thinking"
    :key (lambda () (auth-source-pick-first-password :host "api.anthropic.com"))
    :stream t
    :models '(claude-3-7-sonnet-20250219)
    :header (lambda () 
              (let ((key (gptel--get-api-key 
                          ;; Explicitly use the same key function as defined above
                          (lambda () (auth-source-pick-first-password :host "api.anthropic.com")))))
                `(("x-api-key" . ,key)
                  ("anthropic-version" . "2023-06-01")
                  ("anthropic-beta" . "pdfs-2024-09-25")
                  ("anthropic-beta" . "output-128k-2025-02-19")
                  ("anthropic-beta" . "prompt-caching-2024-07-31"))))
    :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
                      :max_tokens 4096)))

(gptel-make-tool
 :name "create_file"                    ; javascript-style  snake_case name
 :function (lambda (path filename content)   ; the function that runs
             (let ((full-path (expand-file-name filename path)))
               (with-temp-buffer
                 (insert content)
                 (write-file full-path))
               (format "Created file %s in %s" filename path)))
 :description "Create a new file with the specified content"
 :args (list '(:name "path"             ; a list of argument specifications
               :type string
               :description "The directory where to create the file")
             '(:name "filename"
               :type string
               :description "The name of the file to create")
             '(:name "content"
               :type string
               :description "The content to write to the file"))
 :category "filesystem")                ; An arbitrary label for grouping

(use-package elysium
  :straight (:host github :repo "lanceberge/elysium" :branch "main" :files ("*.el")))
