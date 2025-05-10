;; -*- lexical-binding: t; -*-

;; Startup profiling - uncomment to debug startup time
;; (defvar jf/init-start-time (current-time))

;; Start with debugging enabled during development
(setq debug-on-error t)

;; Register shortcut to quickly open this file
(set-register ?i (cons 'file "~/src/dotfiles/emacs/modular_init.org"))

;; Define root directory
(defvar jf/emacs-dir (expand-file-name "~/src/dotfiles/emacs/")
  "The root directory of the Emacs configuration.")

;; Debug mode for troubleshooting
(defvar jf/module-debug nil
  "When non-nil, print extra debug information during module loading.")

;; Module loading function with error handling
(defun jf/load-module (module-path)
  "Load MODULE-PATH with error handling and reporting."
  (when jf/module-debug
    (message "Loading module: %s" module-path))
  
  (let ((start-time (current-time)))
    (condition-case-unless-debug err
        (progn
          (load module-path nil nil t)
          (when jf/module-debug
            (message "Loaded %s in %.3f seconds" 
                     module-path 
                     (float-time (time-subtract (current-time) start-time)))))
      (error
       (message "ERROR in %s: %s" module-path (error-message-string err))
       nil))))

;; Function to resolve a module path to a file path
(defun jf/resolve-module-path (module-path)
  "Convert a MODULE-PATH like 'core/defaults' to a file path."
  (let* ((parts (split-string module-path "/"))
         (dir (car parts))
         (name (cadr parts)))
    (expand-file-name (concat dir "/" name ".el") jf/emacs-dir)))

;; Function to reload a specific module (useful for debugging)
(defun jf/reload-module (module-path)
  "Reload a specific MODULE-PATH for debugging."
  (interactive 
   (list (completing-read "Reload module: " 
                          (mapcar #'car jf/enabled-modules))))
  
  (let ((jf/module-debug t))
    (jf/load-module (jf/resolve-module-path module-path))))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Disable package.el
(setq package-enable-at-startup nil)

;; Install and configure use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Ensure we're using a consistent org version
;; This is important as org-roam depends on specific org versions
;; Force straight to use org
(straight-use-package 'org)

;; Define enabled modules with descriptions
(defvar jf/enabled-modules
  '(
    ;; Core modules - add these as you create them
    ("core/defaults"      "Basic Emacs behavior")
    ("core/evil"          "Evil mode configuration")
    
    ;; Feature modules - use your existing .el files
    ;; ("vertico-consult-embark" "Completion framework")
    
    ;; Language mode modules
    ;; ("language-modes/ide-features" "Shared IDE functionality")
    
    ;; Major mode modules
    ("major-modes/org"    "Org-mode configuration")
    ("major-modes/org-roam" "Org-roam knowledge management")
    ("major-modes/dirvish" "Enhanced directory viewer")
    ("major-modes/magit"  "Git interface")
    ("major-modes/gpt"    "LLM/AI integration")
    )
  "List of enabled modules with their paths and descriptions.")

;; Define machine-specific configurations
(defvar jf/machine-name (system-name)
  "The machine's hostname, used to load machine-specific configurations.")

;; Load all enabled modules
(dolist (module-spec jf/enabled-modules)
  (let ((module-path (car module-spec)))
    (jf/load-module (jf/resolve-module-path module-path))))

;; Load machine-specific configuration if it exists
(let ((machine-config (expand-file-name (concat "local/" jf/machine-name ".el") jf/emacs-dir)))
  (when (file-exists-p machine-config)
    (jf/load-module machine-config)))

;; Reset garbage collection threshold after startup
(setq gc-cons-threshold 2000000) ;; 2MB

;; Report startup time if debugging
(when (boundp 'jf/init-start-time)
  (let ((elapsed (float-time (time-subtract (current-time) jf/init-start-time))))
    (message "Loading Emacs took %.3f seconds" elapsed)))

;; Don't show this init message after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq debug-on-error nil)
            (message "Emacs ready!")))

;; Store customizations in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
