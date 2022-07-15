;; ===============================================================================
;; configure MELPA
;; ===============================================================================

;(require 'package)
;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;(package-initialize)

;; ===============================================================================
;; configure straight.el
;; ===============================================================================

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

(setq package-enable-at-startup nil)

;; install use-package
(straight-use-package 'use-package)

;; ===============================================================================
;; Basic Configuration
;; ===============================================================================

(use-package better-defaults
  :straight t)
(use-package monokai-theme
  :straight t)
(use-package material-theme
  :straight t)
(use-package vs-light-theme
  :straight t)
(use-package magit
  :straight t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; set default frame size
(add-to-list 'default-frame-alist '(height . 74))
(add-to-list 'default-frame-alist '(width . 120))

;; Show line number in the mode line.
(line-number-mode 1)
;; Show column number in the mode line.
(column-number-mode 1)

(load-theme 'material t)

;; there's a bunch of options for this and I don't understand them all
;; https://stackoverflow.com/questions/6874516/relative-line-numbers-in-emas
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; enable saving state after closing emacs
(desktop-save-mode)
(desktop-read)

;; Configure common modes like yaml, json etc
(use-package yaml-mode
  :straight t)

;; ===============================================================================
;; python development config
;; ===============================================================================

(use-package elpy
  :straight t
  :config
  (elpy-enable)
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules)))
(use-package flycheck
  :straight t
  :config
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))
(use-package py-autopep8
  :straight t
  :config
  (add-hook 'python-mode-hook 'py-autopep8-mode))
(use-package blacken
  :straight t)

;; ===============================================================================
;; configure Docker
;; ===============================================================================

;; https://github.com/Silex/docker.el
(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :straight t)

;; ===============================================================================
;; Configure Ansible
;; ===============================================================================

;; ansible uses the yaml-mode configured in the general section

;; ===============================================================================
;; Configure Terraform
;; ===============================================================================

(use-package terraform-mode
  :straight t
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;; ===============================================================================
;; Configure Language Server Protocol (LSP)
;; ===============================================================================

(use-package lsp-mode
  :straight t)

;; ===============================================================================
;; Configure Ido https://www.emacswiki.org/emacs/InteractivelyDoThings
;; ===============================================================================

(use-package ido-vertical-mode
  :straight t
  :config
  (require 'ido)
  (ido-mode t)
  (ido-vertical-mode t)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  (use-package flx-ido
    :straight t)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  (use-package smex
    :straight t)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; ===============================================================================
;; configure evil mode
;; ===============================================================================

(use-package evil
  :straight t
  :config
  (evil-mode 1)
  (add-hook 'org-capture-mode-hook 'evil-insert-state) ;; use insert by default for org todos
  (add-hook 'git-commit-mode-hook 'evil-insert-state) ;; use insert mode by default for magit commits
  (use-package goto-chg
    :straight t))

;; ===============================================================================
;; Configs from pragmaticemacs.wordpress.com Org-Mode TODO
;; ===============================================================================

;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; file to save todo items in
(setq org-agenda-files (quote ("~/todo.org")))

;; set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;; set colors for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
			   (?B . (:foreground "LightSteelBlue"))
			   (?C . (:foreground "OliveDrab"))))

;; open agenda in current window
(setq open-agenda-window-setup (quote current-window))

;; capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/todo.org" "Tasks")
	 "* TODO [#A] %?")))
			      
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "a9318f38c2d39f717d61aa0c155f579fc3a369c4a0d01f4848de0dee85fbd831" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" default))
 '(package-selected-packages
   '(blacken py-autopep8 flycheck elpy better-defaults material-theme vs-light-theme monokai-theme solarized-theme magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
