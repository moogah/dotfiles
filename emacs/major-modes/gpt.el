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

;; Configuration for gptel session auto-save
(defcustom jf/gptel-autosave-enabled t
  "Whether to automatically save gptel sessions after each response."
  :type 'boolean
  :group 'gptel)

(defcustom jf/gptel-sessions-directory "~/gptel-sessions/"
  "Directory for storing gptel sessions.
Will be created if it doesn't exist."
  :type 'directory
  :group 'gptel)

(defcustom jf/gptel-session-filename-format "%Y%m%d-%H%M%S"
  "Format string for timestamp portion of gptel session filenames.
Uses `format-time-string' syntax."
  :type 'string
  :group 'gptel)

(defun jf/gptel--sanitize-model-name (model)
  "Sanitize MODEL symbol for use in filename.
Converts to lowercase, replaces special chars with hyphens."
  (let ((name (symbol-name model)))
    (replace-regexp-in-string
     "-+" "-"  ; collapse multiple hyphens
     (replace-regexp-in-string
      "[^a-z0-9-]" "-"  ; replace special chars
      (downcase name)))))

(defun jf/gptel--generate-session-filename ()
  "Generate filename for current gptel session.
Format: TIMESTAMP-MODELNAME.EXT (extension based on major-mode)"
  (let* ((timestamp (format-time-string jf/gptel-session-filename-format))
         (model-name (jf/gptel--sanitize-model-name gptel-model))
         (extension (cond
                     ((derived-mode-p 'org-mode) "org")
                     ((derived-mode-p 'markdown-mode) "md")
                     (t "txt"))))
    (format "%s-%s.%s" timestamp model-name extension)))

(defun jf/gptel--autosave-session (response-start response-end)
  "Automatically save gptel session after LLM response.
RESPONSE-START and RESPONSE-END mark the response boundaries.
This function is added to `gptel-post-response-functions'."
  (when (and jf/gptel-autosave-enabled
             gptel-mode)
    (condition-case err
        (if buffer-file-name
            ;; Existing session - just save
            (save-buffer)
          ;; New ephemeral session - create file
          (let* ((filename (jf/gptel--generate-session-filename))
                 (sessions-dir (expand-file-name jf/gptel-sessions-directory))
                 (full-path (expand-file-name filename sessions-dir)))
            ;; Ensure directory exists
            (make-directory sessions-dir t)
            ;; Set buffer-file-name and save
            (setq buffer-file-name full-path)
            (set-visited-file-modtime)
            (set-buffer-modified-p t)
            (save-buffer)
            (message "gptel session saved: %s" (file-name-nondirectory full-path))))
      (file-error
       (message "gptel autosave failed: %s" (error-message-string err)))
      (error
       (message "gptel autosave error: %s" (error-message-string err))))))

(defun jf/gptel--get-session-preview (file)
  "Extract preview text from gptel session FILE.
Returns first ~80 chars of user prompt content."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file nil 0 1000)  ; Read first 1000 chars
        (goto-char (point-min))
        ;; Skip properties and directives
        (while (and (not (eobp))
                    (or (looking-at "^:")
                        (looking-at "^#\\+")))
          (forward-line 1))
        ;; Get next non-empty line
        (while (and (not (eobp)) (looking-at "^[[:space:]]*$"))
          (forward-line 1))
        (let ((start (point))
              (end (line-end-position)))
          (buffer-substring start (min end (+ start 80)))))
    (error "...")))

(defun jf/gptel-browse-sessions ()
  "Browse and open saved gptel sessions from directory."
  (interactive)
  (let* ((sessions-dir (expand-file-name jf/gptel-sessions-directory))
         (files (when (file-directory-p sessions-dir)
                  (directory-files sessions-dir t "\\.\(org\\|md\\|txt\\)$")))
         ;; Sort by modification time, most recent first
         (sorted-files (sort files
                             (lambda (a b)
                               (time-less-p (file-attribute-modification-time (file-attributes b))
                                            (file-attribute-modification-time (file-attributes a))))))
         (candidates
          (mapcar
           (lambda (file)
             (let* ((filename (file-name-nondirectory file))
                    ;; Parse filename: TIMESTAMP-MODEL.org
                    (parts (split-string (file-name-sans-extension filename) "-"))
                    (timestamp (if (>= (length parts) 2)
                                   (concat (nth 0 parts) "-" (nth 1 parts))
                                 (nth 0 parts)))
                    (model (if (>= (length parts) 3)
                               (mapconcat 'identity (nthcdr 2 parts) "-")
                             "unknown"))
                    (preview (jf/gptel--get-session-preview file))
                    (display (format "%-17s | %-20s | %s"
                                     timestamp model preview)))
               (cons display file)))
           sorted-files)))

    (if (null candidates)
        (message "No gptel sessions found in %s" sessions-dir)
      (let* ((choice (completing-read "Open gptel session: " candidates nil t))
             (file (cdr (assoc choice candidates))))
        (find-file file)))))

;; Install auto-save hook
(with-eval-after-load 'gptel
  (add-hook 'gptel-post-response-functions #'jf/gptel--autosave-session))

;; TODO: Add keybinding for jf/gptel-browse-sessions
;; For now, access via M-x jf/gptel-browse-sessions

(use-package elysium
  :straight (:host github :repo "lanceberge/elysium" :branch "main" :files ("*.el")))
