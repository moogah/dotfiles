;; -*- lexical-binding: t; -*-

;;  (use-package shell-maker
;;    :straight (shell-maker :type git :host github :repo "xenodium/shell-maker" :files ("shell-maker.el")))
;;
;;  ; add :build (:not compile) if byte code has problems
;;  (use-package chatgpt-shell
;;    :requires shell-maker
;;    :straight (:host github :repo "xenodium/chatgpt-shell" :files ("*.el") :build (:not compile)))
;;
;;  (setq chatgpt-shell-openai-key
;;        (lambda ()
;;          (auth-source-pick-first-password :host "api.openai.com")))
;;
;;  ;;(setq chatgpt-shell-model-version "gpt-4o")

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
                      :max_tokens 4096))

  ;; Configure ChatGPT/OpenAI backend
  (gptel-make-openai "ChatGPT"
    :key (lambda () (auth-source-pick-first-password :host "api.openai.com"))
    :stream t
    :models '(gpt-4o gpt-4o-mini gpt-4-turbo gpt-3.5-turbo)))

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

(defun jf/gptel-gpt4o ()
  "Launch gptel session with GPT-4o in a dedicated buffer."
  (interactive)
  (let ((backend (alist-get "ChatGPT" gptel--known-backends nil nil #'equal))
        (model 'gpt-4o))
    (pop-to-buffer (gptel "*gptel-gpt4o*" nil nil t))
    (setq-local gptel-backend backend)
    (setq-local gptel-model model)))

(defun jf/gptel-gpt4o-mini ()
  "Launch gptel session with GPT-4o-mini in a dedicated buffer."
  (interactive)
  (let ((backend (alist-get "ChatGPT" gptel--known-backends nil nil #'equal))
        (model 'gpt-4o-mini))
    (pop-to-buffer (gptel "*gptel-gpt4o-mini*" nil nil t))
    (setq-local gptel-backend backend)
    (setq-local gptel-model model)))

(defun jf/gptel-claude-thinking ()
  "Launch gptel session with Claude Sonnet (extended thinking) in a dedicated buffer."
  (interactive)
  (let ((backend (alist-get "Claude-thinking" gptel--known-backends nil nil #'equal))
        (model 'claude-3-7-sonnet-20250219))
    (pop-to-buffer (gptel "*gptel-claude-thinking*" nil nil t))
    (setq-local gptel-backend backend)
    (setq-local gptel-model model)))

(defun jf/gptel-perplexity ()
  "Launch gptel session with Perplexity in a dedicated buffer."
  (interactive)
  (let ((backend (alist-get "Perplexity" gptel--known-backends nil nil #'equal))
        (model 'sonar))
    (pop-to-buffer (gptel "*gptel-perplexity*" nil nil t))
    (setq-local gptel-backend backend)
    (setq-local gptel-model model)))

;; Keybindings for gptel launchers
;; Uses <SPC> l prefix (l for LLM)
(with-eval-after-load 'gptel
  ;; Define prefix command map for LLM launchers
  (define-prefix-command 'jf/gptel-launcher-map)
  (evil-define-key 'normal 'global (kbd "<SPC> l") 'jf/gptel-launcher-map)

  ;; Individual launcher bindings
  (define-key jf/gptel-launcher-map (kbd "4") 'jf/gptel-gpt4o)
  (define-key jf/gptel-launcher-map (kbd "m") 'jf/gptel-gpt4o-mini)
  (define-key jf/gptel-launcher-map (kbd "c") 'jf/gptel-claude-thinking)
  (define-key jf/gptel-launcher-map (kbd "p") 'jf/gptel-perplexity))

(use-package elysium
  :straight (:host github :repo "lanceberge/elysium" :branch "main" :files ("*.el")))
