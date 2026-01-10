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

(defun jf/gptel--create-metadata (session-dir session-id model backend)
  "Create initial metadata structure for a new session.
SESSION-DIR is the directory path, SESSION-ID is the session identifier,
MODEL is the model symbol, BACKEND is the backend name."
  (list :session_id session-id
        :model (symbol-name model)
        :backend backend
        :created (format-time-string "%Y-%m-%dT%H:%M:%SZ")
        :tree (list :id "root"
                    :children [])
        :current_path ["root"]))

(defun jf/gptel--read-metadata (session-dir)
  "Read metadata.json from SESSION-DIR.
Returns metadata plist or nil if file doesn't exist."
  (let ((metadata-file (expand-file-name "metadata.json" session-dir)))
    (when (file-exists-p metadata-file)
      (condition-case nil
          (let ((json-object-type 'plist)
                (json-array-type 'vector)
                (json-key-type 'keyword))
            (json-read-file metadata-file))
        (error nil)))))

(defun jf/gptel--write-metadata (session-dir metadata)
  "Write METADATA plist to metadata.json in SESSION-DIR."
  (let ((metadata-file (expand-file-name "metadata.json" session-dir))
        (json-encoding-pretty-print t))
    (with-temp-file metadata-file
      (insert (json-encode metadata)))))

(defun jf/gptel--add-tree-node (tree parent-path node-id node-type filename preview)
  "Add a new node to TREE at PARENT-PATH.
NODE-ID is the node identifier, NODE-TYPE is 'message' or 'response',
FILENAME is the file containing this node's content,
PREVIEW is a short preview of the content.
Returns the updated tree (modified in place for nested structures)."
  (let ((new-node (list :id node-id
                        :type (symbol-name node-type)
                        :file filename
                        :timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ")
                        :preview preview
                        :children [])))
    (if (and (vectorp parent-path) (equal parent-path ["root"]))
        ;; Add to root's children
        (progn
          (plist-put tree :children
                     (vconcat (plist-get tree :children) (vector new-node)))
          tree)
      ;; Navigate to parent and add to its children
      (jf/gptel--add-node-recursive tree (coerce parent-path 'list) new-node 1)
      tree)))

(defun jf/gptel--add-node-recursive (tree path new-node depth)
  "Recursively find parent node in TREE following PATH and add NEW-NODE.
DEPTH tracks current position in path (skipping 'root')."
  (if (>= depth (length path))
      ;; Reached parent, add new-node to its children
      (plist-put tree :children
                 (vconcat (plist-get tree :children) (vector new-node)))
    ;; Navigate deeper
    (let* ((target-id (nth depth path))
           (children (plist-get tree :children))
           (child-found nil))
      (seq-doseq (child children)
        (when (equal (plist-get child :id) (format "%s" target-id))
          (setq child-found t)
          (jf/gptel--add-node-recursive child path new-node (1+ depth))))
      (unless child-found
        (error "Could not find node %s in tree" target-id)))))

(defun jf/gptel--find-leaf-nodes (tree &optional path)
  "Find all leaf nodes in TREE.
Returns list of (path . node) pairs where path is vector of node IDs."
  (let ((current-path (or path ["root"]))
        (children (plist-get tree :children))
        leaves nil)
    (if (or (null children) (zerop (length children)))
        ;; This is a leaf
        (list (cons current-path tree))
      ;; Recurse into children
      (seq-mapcat
       (lambda (child)
         (jf/gptel--find-leaf-nodes
          child
          (vconcat current-path (vector (plist-get child :id)))))
       children))))

(defun jf/gptel--get-conversation-path (metadata)
  "Get the current conversation path from METADATA.
Returns vector of node IDs representing the current branch."
  (plist-get metadata :current_path))

(defun jf/gptel--sanitize-model-name (model)
  "Sanitize MODEL symbol for use in filename.
Converts to lowercase, replaces special chars with hyphens."
  (let ((name (symbol-name model)))
    (replace-regexp-in-string
     "-+" "-"  ; collapse multiple hyphens
     (replace-regexp-in-string
      "[^a-z0-9-]" "-"  ; replace special chars
      (downcase name)))))

(defun jf/gptel--generate-session-dirname ()
  "Generate directory name for current gptel session.
Format: TIMESTAMP-MODELNAME"
  (let* ((timestamp (format-time-string jf/gptel-session-filename-format))
         (model-name (jf/gptel--sanitize-model-name gptel-model)))
    (format "%s-%s" timestamp model-name)))

(defun jf/gptel--get-file-extension ()
  "Get file extension based on current major-mode."
  (cond
   ((derived-mode-p 'org-mode) "org")
   ((derived-mode-p 'markdown-mode) "md")
   (t "txt")))

(defun jf/gptel--get-next-message-id (metadata)
  "Get the next message ID from METADATA.
Returns format 'message-N' or 'message-N-altM' for forks."
  (let* ((current-path (plist-get metadata :current_path))
         (last-node-id (when (> (length current-path) 1)
                         (aref current-path (1- (length current-path)))))
         ;; Extract number from last node (e.g., "response-2" -> 2)
         (last-num (when last-node-id
                     (if (string-match "\\([0-9]+\\)" (format "%s" last-node-id))
                         (string-to-number (match-string 1 (format "%s" last-node-id)))
                       0)))
         (next-num (if last-num (1+ last-num) 1)))
    (format "message-%d" next-num)))

;; Buffer-local variables to track session state
(defvar-local jf/gptel--session-dir nil
  "Directory where current session is stored.")

(defvar-local jf/gptel--session-metadata nil
  "Metadata plist for current session.")

(defvar-local jf/gptel--message-counter 0
  "Counter for message/response pairs in current session.")

(defun jf/gptel--autosave-session (response-start response-end)
  "Automatically save gptel session after LLM response.
RESPONSE-START and RESPONSE-END mark the response boundaries.
This function is added to `gptel-post-response-functions'."
  (when (and jf/gptel-autosave-enabled
             gptel-mode)
    (condition-case err
        (progn
          ;; Initialize session if needed
          (unless jf/gptel--session-dir
            (jf/gptel--initialize-session))

          ;; Determine message ID (fork or sequential)
          (let* ((ext (jf/gptel--get-file-extension))
                 (msg-id (if (bound-and-true-p jf/gptel--forking-next)
                             ;; Create fork ID
                             (prog1
                                 (jf/gptel--get-next-fork-id
                                  jf/gptel--session-metadata
                                  (plist-get jf/gptel--session-metadata :current_path))
                               ;; Clear fork flag
                               (setq jf/gptel--forking-next nil))
                           ;; Normal sequential ID
                           (progn
                             (setq jf/gptel--message-counter (1+ jf/gptel--message-counter))
                             (format "message-%d" jf/gptel--message-counter))))
                 ;; Response ID matches message number
                 (resp-id (replace-regexp-in-string "message-" "response-" msg-id))
                 (msg-file (format "%s.%s" msg-id ext))
                 (resp-file (format "%s.%s" resp-id ext))
                 ;; For now, extract the last message and response
                 ;; TODO: Improve to extract exact content
                 (message-content (jf/gptel--extract-last-message response-start))
                 (response-content (buffer-substring response-start response-end))
                 (msg-preview (substring message-content 0 (min 80 (length message-content))))
                 (resp-preview (substring response-content 0 (min 80 (length response-content)))))

            ;; Save message file
            (with-temp-file (expand-file-name msg-file jf/gptel--session-dir)
              (insert message-content))

            ;; Save response file
            (with-temp-file (expand-file-name resp-file jf/gptel--session-dir)
              (insert response-content))

            ;; Update metadata tree
            (jf/gptel--update-metadata-tree msg-id msg-file msg-preview
                                            resp-id resp-file resp-preview)

            (message "Session saved: %s/%s"
                     (file-name-nondirectory jf/gptel--session-dir)
                     resp-file)))
      (file-error
       (message "gptel autosave failed: %s" (error-message-string err)))
      (error
       (message "gptel autosave error: %s" (error-message-string err))))))

(defun jf/gptel--initialize-session ()
  "Initialize a new session directory and metadata."
  (let* ((dirname (jf/gptel--generate-session-dirname))
         (sessions-base (expand-file-name jf/gptel-sessions-directory))
         (session-dir (expand-file-name dirname sessions-base))
         (backend-name (gptel-backend-name gptel-backend)))
    ;; Create session directory
    (make-directory session-dir t)

    ;; Create initial metadata
    (setq jf/gptel--session-metadata
          (jf/gptel--create-metadata session-dir dirname gptel-model backend-name))

    ;; Write metadata
    (jf/gptel--write-metadata session-dir jf/gptel--session-metadata)

    ;; Store directory
    (setq jf/gptel--session-dir session-dir)

    (message "Created session: %s" dirname)))

(defun jf/gptel--extract-last-message (response-start)
  "Extract the user's last message before RESPONSE-START."
  ;; Find the last occurrence of gptel-prompt-prefix-string before response-start
  (save-excursion
    (goto-char response-start)
    (let ((prompt-start (if (boundp 'gptel-prompt-prefix-string)
                            (search-backward gptel-prompt-prefix-string nil t)
                          (previous-single-property-change (point) 'gptel)))
          (prompt-end response-start))
      (if prompt-start
          (buffer-substring (+ prompt-start (if (boundp 'gptel-prompt-prefix-string)
                                                 (length gptel-prompt-prefix-string)
                                               0))
                            prompt-end)
        ;; Fallback: get last 500 chars before response
        (buffer-substring (max (point-min) (- response-start 500)) response-start)))))

(defun jf/gptel--update-metadata-tree (msg-id msg-file msg-preview resp-id resp-file resp-preview)
  "Update metadata tree with new message and response nodes."
  (let* ((tree (plist-get jf/gptel--session-metadata :tree))
         (current-path (plist-get jf/gptel--session-metadata :current_path))
         ;; Add message node
         (tree-with-msg (jf/gptel--add-tree-node tree current-path msg-id 'message msg-file msg-preview))
         ;; Add response node (child of message)
         (new-msg-path (vconcat current-path (vector msg-id)))
         (tree-with-resp (jf/gptel--add-tree-node tree-with-msg new-msg-path resp-id 'response resp-file resp-preview))
         ;; Update current path
         (new-path (vconcat new-msg-path (vector resp-id))))

    ;; Update metadata
    (plist-put jf/gptel--session-metadata :tree tree-with-resp)
    (plist-put jf/gptel--session-metadata :current_path new-path)

    ;; Write to disk
    (jf/gptel--write-metadata jf/gptel--session-dir jf/gptel--session-metadata)))

(defun jf/gptel-fork-session ()
  "Mark current position as fork point for next message.
Next message sent will create a branch (e.g., message-3-alt1)."
  (interactive)
  (unless (and jf/gptel--session-dir jf/gptel--session-metadata)
    (user-error "No active session to fork"))

  ;; Mark as forked by setting a buffer-local flag
  (setq-local jf/gptel--forking-next t)
  (message "Next message will create a new branch"))

(defun jf/gptel--get-next-fork-id (metadata parent-path)
  "Get next fork ID (e.g., 'message-3-alt1') based on existing forks.
METADATA is the session metadata, PARENT-PATH is where to fork from."
  (let* ((tree (plist-get metadata :tree))
         ;; Find parent node
         (parent-node (jf/gptel--find-node-by-path tree parent-path))
         (parent-children (plist-get parent-node :children))
         ;; Get last response number from parent path
         (last-id (aref parent-path (1- (length parent-path))))
         (base-num (if (string-match "\\([0-9]+\\)" (format "%s" last-id))
                       (string-to-number (match-string 1 (format "%s" last-id)))
                     0))
         (next-num (1+ base-num))
         ;; Find existing alts
         (existing-alts
          (seq-filter
           (lambda (child)
             (string-match (format "message-%d-alt\\([0-9]+\\)" next-num)
                          (format "%s" (plist-get child :id))))
           parent-children))
         (next-alt-num (1+ (length existing-alts))))
    (format "message-%d-alt%d" next-num next-alt-num)))

(defun jf/gptel--find-node-by-path (tree path)
  "Find node in TREE by following PATH (vector of node IDs)."
  (if (or (null path) (equal path ["root"]))
      tree
    (jf/gptel--find-node-recursive tree (coerce path 'list) 1)))

(defun jf/gptel--find-node-recursive (tree path depth)
  "Recursively find node in TREE following PATH at DEPTH."
  (if (>= depth (length path))
      tree
    (let* ((target-id (nth depth path))
           (children (plist-get tree :children))
           (found nil))
      (seq-doseq (child children)
        (when (equal (plist-get child :id) (format "%s" target-id))
          (setq found (jf/gptel--find-node-recursive child path (1+ depth)))))
      found)))

(defun jf/gptel-browse-sessions ()
  "Browse and open saved gptel sessions from directory.
Shows leaf nodes (endpoints) of each branch."
  (interactive)
  (let* ((sessions-dir (expand-file-name jf/gptel-sessions-directory))
         (session-dirs (when (file-directory-p sessions-dir)
                         (seq-filter #'file-directory-p
                                    (directory-files sessions-dir t "^[^.]"))))
         (candidates (jf/gptel--build-session-candidates session-dirs)))

    (if (null candidates)
        (message "No gptel sessions found in %s" sessions-dir)
      (let* ((choice (completing-read "Open session branch: " (mapcar #'car candidates) nil t))
             (session-info (cdr (assoc choice candidates))))
        (jf/gptel--open-session-branch session-info)))))

(defun jf/gptel--build-session-candidates (session-dirs)
  "Build list of session branch candidates from SESSION-DIRS.
Returns alist of (display-string . (session-dir path leaf-node))."
  (let (candidates)
    (dolist (dir session-dirs)
      (let ((metadata (jf/gptel--read-metadata dir)))
        (when metadata
          (let* ((tree (plist-get metadata :tree))
                 (leaves (jf/gptel--find-leaf-nodes tree))
                 (session-id (plist-get metadata :session_id)))
            ;; Add candidate for each leaf
            (dolist (leaf leaves)
              (let* ((path (car leaf))
                     (node (cdr leaf))
                     (node-id (plist-get node :id))
                     (preview (plist-get node :preview))
                     (timestamp (plist-get node :timestamp))
                     (branch-label (if (string-match "alt\\([0-9]+\\)" (format "%s" node-id))
                                       (format " [branch %s]" (match-string 1 (format "%s" node-id)))
                                     ""))
                     (display (format "%-30s%s | %s"
                                      session-id
                                      branch-label
                                      (or preview "..."))))
                (push (cons display (list :dir dir :path path :node node)) candidates))))))
    (nreverse candidates)))

(defun jf/gptel--open-session-branch (session-info)
  "Open a session branch and reconstruct conversation.
SESSION-INFO is a plist with :dir, :path, and :node."
  (let* ((dir (plist-get session-info :dir))
         (path (plist-get session-info :path))
         (metadata (jf/gptel--read-metadata dir))
         (tree (plist-get metadata :tree)))
    ;; Reconstruct conversation from root to this leaf
    (jf/gptel--reconstruct-conversation dir tree path)))

(defun jf/gptel--reconstruct-conversation (session-dir tree path)
  "Reconstruct conversation by reading files along PATH in TREE.
Creates a new gptel buffer with the conversation and sets up state."
  (let* ((metadata (jf/gptel--read-metadata session-dir))
         (model-name (plist-get metadata :model))
         (backend-name (plist-get metadata :backend))
         (session-id (plist-get metadata :session_id))
         (buffer-name (format "*gptel-%s*" session-id))
         (conversation-parts '())
         ;; Convert path to list of node IDs
         (node-ids (coerce path 'list)))

    ;; Walk the path and collect file contents
    (jf/gptel--walk-path-and-collect tree node-ids session-dir conversation-parts)

    ;; Create or switch to buffer
    (let ((buf (get-buffer-create buffer-name)))
      (with-current-buffer buf
        ;; Set major mode (use markdown by default, could be smarter)
        (markdown-mode)
        (gptel-mode 1)

        ;; Clear buffer and insert conversation
        (erase-buffer)
        (dolist (part (nreverse conversation-parts))
          (insert part))

        ;; Set up session state
        (setq jf/gptel--session-dir session-dir)
        (setq jf/gptel--session-metadata metadata)
        ;; Update current path in metadata
        (plist-put jf/gptel--session-metadata :current_path path)
        (jf/gptel--write-metadata session-dir jf/gptel--session-metadata)

        ;; Extract counter from last message
        (let ((last-msg-id (car (last node-ids))))
          (when (string-match "\\([0-9]+\\)" (format "%s" last-msg-id))
            (setq jf/gptel--message-counter
                  (string-to-number (match-string 1 (format "%s" last-msg-id))))))

        ;; Set up gptel backend and model
        ;; TODO: Restore correct backend/model from metadata

        (goto-char (point-max)))

      ;; Display buffer
      (pop-to-buffer buf)
      (message "Loaded session: %s (at %s)" session-id (car (last node-ids))))))

(defun jf/gptel--walk-path-and-collect (tree node-ids session-dir parts-list)
  "Walk TREE following NODE-IDS and collect file contents from SESSION-DIR.
Accumulates content in PARTS-LIST."
  (when (> (length node-ids) 1) ; Skip "root"
    (jf/gptel--walk-recursive tree (cdr node-ids) session-dir parts-list)))

(defun jf/gptel--walk-recursive (node remaining-ids session-dir parts-list)
  "Recursively walk NODE following REMAINING-IDS."
  (when remaining-ids
    (let* ((target-id (car remaining-ids))
           (children (plist-get node :children))
           (found-child nil))
      (seq-doseq (child children)
        (when (equal (plist-get child :id) (format "%s" target-id))
          (setq found-child child)
          ;; Read this node's file
          (let ((file (plist-get child :file)))
            (when file
              (let ((full-path (expand-file-name file session-dir)))
                (when (file-exists-p full-path)
                  (with-temp-buffer
                    (insert-file-contents full-path)
                    (push (buffer-string) parts-list))))))
          ;; Continue to next node
          (jf/gptel--walk-recursive child (cdr remaining-ids) session-dir parts-list)))))))

;; Install auto-save hook
(with-eval-after-load 'gptel
  (add-hook 'gptel-post-response-functions #'jf/gptel--autosave-session))

;; TODO: Add keybinding for jf/gptel-browse-sessions
;; For now, access via M-x jf/gptel-browse-sessions

(use-package elysium
  :straight (:host github :repo "lanceberge/elysium" :branch "main" :files ("*.el")))
