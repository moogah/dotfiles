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

(defun jf/gptel-launcher ()
  "Launch gptel session with a selected backend and model.
Prompts for display method, then backend:model selection using
completing-read with all available options from gptel's configuration."
  (interactive)
  ;; First, ask where to display the buffer
  (let* ((display-options '(("Current window" . current)
                            ("New tab" . tab)
                            ("Split window" . split)))
         (display-choice (completing-read "Where to open? "
                                          (mapcar #'car display-options)
                                          nil t nil nil "Current window"))
         (display-method (cdr (assoc display-choice display-options)))
         ;; Build models-alist similar to gptel--infix-provider
         (models-alist
          (cl-loop
           for (name . backend) in gptel--known-backends
           nconc (cl-loop for model in (gptel-backend-models backend)
                          collect (list (concat name ":" (gptel--model-name model))
                                        backend model))))
         (selected (completing-read "Select model: "
                                     (mapcar #'car models-alist)
                                     nil t))
         (choice (assoc selected models-alist))
         (backend (nth 1 choice))
         (model (nth 2 choice))
         (buffer-name (format "*gptel-%s*" (replace-regexp-in-string ":" "-" selected)))
         ;; Create buffer without displaying it (pass nil for interactivep)
         ;; Note: When interactivep is t, gptel calls (display-buffer) with
         ;; gptel-display-buffer-action, which would display the buffer before
         ;; we can control where it goes. By passing nil, we handle all display
         ;; logic ourselves based on the user's selection.
         (buffer (gptel buffer-name nil nil nil)))
    ;; Display buffer based on user's choice
    (pcase display-method
      ('split (pop-to-buffer buffer))
      ('tab (tab-bar-new-tab)
            (switch-to-buffer buffer))
      ('current (switch-to-buffer buffer)))
    ;; Set backend and model as buffer-local
    (with-current-buffer buffer
      (setq-local gptel-backend backend)
      (setq-local gptel-model model))))

;; Keybinding for gptel launcher
;; Direct access with <SPC> l
(with-eval-after-load 'gptel
  (evil-define-key 'normal 'global (kbd "<SPC> l") 'jf/gptel-launcher))

(use-package elysium
  :straight (:host github :repo "lanceberge/elysium" :branch "main" :files ("*.el")))
