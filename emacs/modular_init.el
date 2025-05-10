;; -*- lexical-binding: t; -*-

;; Startup profiling - uncomment to debug startup time
;; (defvar jf/init-start-time (current-time))

;; Start with debugging enabled during development
(setq debug-on-error t)

;; Register shortcut to quickly open this file
(set-register ?i (cons 'file "~/src/dotfiles/emacs/modular_init.el"))

;; Define root directory
(defvar jf/emacs-dir (expand-file-name "~/src/dotfiles/emacs/")
  "The root directory of the Emacs configuration.")

;; Define configuration directories with categories
(defvar jf/config-dirs
  '((:core    . ,(expand-file-name "core/" jf/emacs-dir))
    (:modules . ,(expand-file-name "modules/" jf/emacs-dir))
    (:lang    . ,(expand-file-name "language-modes/" jf/emacs-dir))
    (:major   . ,(expand-file-name "major-modes/" jf/emacs-dir))
    (:local   . ,(expand-file-name "local/" jf/emacs-dir)))
  "Mapping of configuration categories to their directories.")

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

;; Function to get module directory based on its type
(defun jf/get-module-dir (module-name)
  "Get the appropriate directory for MODULE-NAME."
  (let* ((module-data (seq-find (lambda (m) (string= (nth 1 m) module-name)) jf/modules))
         (category (if module-data (car module-data) :modules)))
    (alist-get category jf/config-dirs)))

;; Function to reload a specific module (useful for debugging)
(defun jf/reload-module (module)
  "Reload a specific MODULE for debugging."
  (interactive 
   (list (completing-read "Reload module: " jf/enabled-modules)))
  
  (let ((jf/module-debug t)
        (module-dir (jf/get-module-dir module)))
    (jf/load-module (expand-file-name (concat module ".el") module-dir))))

;; Define module configuration - adapt to your modules
(defvar jf/modules
  '(
    ;; Core modules - these should be enabled by default
    (:core "defaults"      "Basic Emacs behavior")
    
    ;; Feature modules - these can be toggled
    (:modules "evil"          "Evil mode")
    (:modules "vertico-consult" "Vertico and Consult")
    
    ;; Language modules
    (:lang "ide-features"  "Shared IDE functionality")
    
    ;; Major mode modules
    (:major "org"          "Org-mode configuration")
    (:major "magit"        "Git interface")
    )
  "List of modules with their type and description.")

;; Extract a list of module names for completion
(defvar jf/enabled-modules
  (mapcar (lambda (module) (nth 1 module)) jf/modules)
  "List of enabled module names.")

;; Define machine-specific configurations
(defvar jf/machine-name (system-name)
  "The machine's hostname, used to load machine-specific configurations.")

;; ===============================================================================
;; Package Management Setup
;; ===============================================================================

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

;; ===============================================================================
;; Load Example Module
;; ===============================================================================

;; For example, load just one module to demonstrate the system
(jf/load-module (expand-file-name "evil.el" jf/emacs-dir))

;; Load machine-specific configuration if it exists
(let ((machine-config (expand-file-name (concat jf/machine-name ".el") 
                                        (alist-get :local jf/config-dirs))))
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