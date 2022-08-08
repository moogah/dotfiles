;; -*- lexical-binding: t; -*-

(toggle-debug-on-error)

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
;; OSX Specific Configs
;; ===============================================================================

;; make sure we have access to the same PATH as in our zsh
(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

;; ===============================================================================
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Look and Feel
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ===============================================================================

(unless (memq window-system '(mac ns))
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; use short answers ie y, n instead of yes, no
(if (boundp 'use-short-answers)
    (setq use-short-answers t))
  
;; automatically update buffers when their state-on-disk changes
(global-auto-revert-mode 1)
;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Do not saves duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; Add sidebar via imenu
(use-package imenu-list
  :straight t
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t))

;; ===============================================================================
;; Configure default frame size, line num display, desktop mode
;; ===============================================================================

; set default frame size
(add-to-list 'default-frame-alist '(height . 65))
(add-to-list 'default-frame-alist '(width . 106))

;; Show line and column number in the mode line.
(line-number-mode 1)
(column-number-mode 1)

;; set relative line number
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; ===============================================================================
;; Customize Modeline
;; ===============================================================================

(use-package telephone-line
  :straight t
  :config
 (telephone-line-mode 1))

;;(use-package doom-modeline
;;  :straight t
;;  :config
;;  (doom-modeline-mode 1))

;;(use-package powerline
;;  :straight (powerline :type git :host github :repo "milkypostman/powerline")
;;  :config
;;  (powerline-default-theme))

;; ===============================================================================
;; install themes
;; ===============================================================================

(use-package monokai-theme
  :straight t)
(use-package material-theme
  :straight t)
(use-package vs-light-theme
  :straight t)
(use-package color-theme-sanityinc-tomorrow
  :straight t)
(use-package cyberpunk-theme
  :straight t)
(use-package afternoon-theme
  :straight t)
(use-package darkburn-theme
  :straight t)
(use-package distinguished-theme
  :straight t)
(use-package doom-themes
  :straight t)
(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme"))

;; set default theme
;; doom-henna
;; doom-peacock
;; doom-vibrant
;; doom-ir
;; doom-old-hope
;; doom-ephemeral
;; doom-laserwave
;; doom-moonlight
;; doom-palenight
(load-theme 'doom-palenight t)
(set-background-color "black")

;; set font size to 14pt for my aging eyes
(setq default-frame-alist '((font . "Menlo-14")))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; ===============================================================================
;; Configure terminal/shell emulations
;; ===============================================================================

; Configure compilation output to handle color terminal output
(use-package xterm-color
  :straight t
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))

;; ===============================================================================
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Major Modes, Global Behaviors Etc..
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ===============================================================================

;; ===============================================================================
;; install magit
;; ===============================================================================

(use-package magit
  :straight t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; Configure common modes like yaml, json etc
(use-package yaml-mode
  :straight t)

(use-package wgrep
  :straight t)

;; ===============================================================================
;; configure dired
;; ===============================================================================

(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-bookmark-entries
   '(("h" "~/"        "Home")
     ("s" "~/src/"    "Source Code")))
  (dirvish-mode-line-format
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (dirvish-attributes '(vscode-icon file-size collapse subtree-state vc-state))
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  (use-package vscode-icon
    :straight t
    :config
    (setq dirvish-vscode-icon-size 18)) ;; vs-code icons is an alternative
  ;;(dirvish-peek-mode)
  (setq dirvish-vscode-icon-size 18)
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  ;;(setq dired-mouse-drag-files t)                   ; added in emacs 29
  ;;(setq mouse-drag-and-drop-region-cross-program t) ; added in emacs 29
  (setq dired-listed-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (setf dired-kill-when-opening-new-dired-buffer t) ;; https://stackoverflow.com/questions/1839313/how-do-i-stop-emacs-dired-mode-from-opening-so-many-buffers
  :bind
  (("C-x d" . dired-jump)
   ("C-c f" . dirvish-fd)
   :map dirvish-mode-map
   ("h"   . dired-up-directory)
   ("j"   . dired-next-line)
   ("k"   . dired-previous-line)
   ("l"   . dired-find-file)
   ("i"   . wdired-change-to-wdired-mode)
   ("."   . dired-omit-mode)
   ("b"   . dirvish-bookmark-jump)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("H"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("TAB" . dirvish-subtree-toggle)
   ("M-n" . dirvish-history-go-forward)
   ("M-p" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(put 'dired-find-alternate-file 'disabled nil)

;; automatically update dired buffers when state-on-disk changes
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; ===============================================================================
;; Window and Frame manager configs
;; ===============================================================================

(use-package perspective
  :straight t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (persp-mode))

;; ===============================================================================
;; Configure Company Auto-Completion
;; ===============================================================================

(use-package company
  :straight t
  :config
  (company-mode)
  (add-hook 'after-init-hook 'global-company-mode))

;; ===============================================================================
;; Configure Hydra
;; ===============================================================================

(use-package hydra
  :straight t)

(defhydra hydra-main (:exit t)
  "
Global Mode Launcher

_p_: Perspective     _R_: rgrep
_P_: Projectile
_r_: Org-Roam
_a_: Org-Agenda
_c_: Consult
"
  ("p" hydra-perspective/body nil)
  ("P" hydra-projectile/body nil)
  ("r" hydra-roam/body nil)
  ("a" hydra-agenda/body nil)
  ("c" hydra-consult/body nil)
  ("R" rgrep nil)
  ("q" nil "cancel"))

(defhydra hydra-perspective (:exit t)
  "
Perspective Mode

_s_: switch perspective    _S_: save
_k_: kill perspective      _L_: load
"
  ("s" persp-switch nil)
  ("S" persp-state-save nil)
  ("k" persp-kill nil)
  ("L" persp-state-load nil)
  ("H" hydra-main/body "Home")
  ("q" nil "cancel"))

(defhydra hydra-projectile (:exit t)
  "
Projectile Mode

_t_: run tests
_k_: kill proj buffers
"
  ("t" projectile-test-project nil)
  ("k" projectile-kill-buffers nil)
  ("H" hydra-main/body "Home")
  ("q" nil "cancel"))

(defhydra hydra-roam (:exit t)
  "
Org-Roam

_r_: add ref
_t_: toggle buffer
"
  ("r" org-roam-ref-add nil)
  ("t" org-roam-buffer-toggle nil)
  ("H" hydra-main/body nil)
  ("q" nil "cancel"))

(defhydra hydra-agenda (:exit t)
  "
Org-Agenda
"
  ("H" hydra-main/body "Home")
  ("q" nil "cancel"))

(defhydra hydra-consult (:exit t)
  "
Consult

_r_: ripgrep       _w_: open buffer in another window
_i_: imenu         _f_: open buffer in another frame
_b_: bookmark
_y_: yank
"
  ("r" consult-ripgrep nil)
  ("i" consult-imenu-multi nil)
  ("b" consult-bookmark nil)
  ("y" consult-yank-from-kill-ring nil)
  ("w" consult-buffer-other-window nil)
  ("f" consult-buffer-other-frame nil)
  ("H" hydra-main/body "Home")
  ("q" nil "cancel"))

(define-key global-map (kbd "C-c h") 'hydra-main/body)

(defhydra hydra-dired (:exit t)
  "
Dired Mode

_b_: bookmarks
"
  ("b" dirvish-bookmark-jump nil)
  ("H" hydra-main/body "Home")
  ("q" nil "cancel"))

(define-key dired-mode-map (kbd "C-c h") 'hydra-dired/body)
;;(define-key org-mode-map (kbd "C-c h") 'hydra-roam/body) ;;


;; ===============================================================================
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Org Mode Configuration
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ===============================================================================

;; ===============================================================================
;; Configure Org-Roam
;; ===============================================================================

(use-package org-roam
  :straight t
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (setq org-roam-directory (file-truename "~/org-roam"))
  (setq find-file-visit-truename t)
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode))

;; ===============================================================================
;; Org Export Engine Config
;; ===============================================================================

;; Jira Export
(use-package ox-jira
  :straight t
  :config
  (setq org-export-copy-to-kill-ring 'if-interactive))


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
	 "* TODO [#A] %?" :empty-lines-before 1)))

(use-package orgit
  :straight (orgit :type git :host github :repo "magit/orgit"))

;;(use-package org-modern
;;  :straight t
;;  :config
;;  (global-org-modern-mode))

;; ===============================================================================
;; Configure PDF Tools
;; ===============================================================================

(use-package pdf-tools
  :straight t; (pdf-tools :type git :host github :repo "vedang/pdf-tools")
  :config
  (setenv "PKG_CONFIG_PATH" "${PKG_CONFIG_PATH}:/opt/homebrew/bin/pkg-config:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
  (pdf-loader-install))

;; ===============================================================================
;; Configure Hyperbole
;; ===============================================================================

;; Running this before configuring capture templates would break the capture

(use-package hyperbole
  :straight t
  :bind
  ("C-c j" . hycontrol-frame-resize-to-left)
  ("C-c k" . hycontrol-frame-resize-to-right)
  :config
  (hyperbole-mode 1))

;; ===============================================================================
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Development Configuration
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ===============================================================================

;; ===============================================================================
;; Configure Tree Sitter
;; ===============================================================================

(use-package tree-sitter
  :straight t
  :hook ((python-mode . tree-sitter-mode)
         (python-mode . tree-sitter-hl-mode)
         (js-mode . tree-sitter-mode)
         (js-mode . tree-sitter-hl-mode)
         (javascript-mode . tree-sitter-mode)
         (javascript-mode . tree-sitter-hl-mode)
         (sh-mode . tree-sitter-mode)
         (sh-mode . tree-sitter-hl-mode)
         (php-mode . tree-sitter-mode)
         (php-mode . tree-sitter-hl-mode)
         (hcl-mode . tree-sitter-mode)
         (hcl-mode . tree-sitter-hl-mode)
         (terraform-mode . tree-sitter-mode)
         (terraform-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :straight t)

(use-package evil-textobj-tree-sitter
  :straight t)

(use-package combobulate
  :straight (combobulate :type git :host github :repo "mickeynp/combobulate")
  :hook ((python-mode . combobulate-mode)
         (js-mode . combobulate-mode)
         (typescript-mode . combobulate-mode)))

;; ===============================================================================
;; Configure markdown mode
;; ===============================================================================

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; ===============================================================================
;; Configure Yasnippet
;; ===============================================================================

(use-package yasnippet
  :straight t
  :config
  (use-package yasnippet-snippets
    :straight t)
    (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"                                       ;; personal snippets
          "~/.emacs.d/straight/repos/yasnippet-snippets/snippets"))   ;; yasnippet snippets
  (yas-global-mode 1))

(use-package auto-yasnippet
  :straight t)
;; @TODO
;; bind aya-create and aya-expand

;; ===============================================================================
;; Configure Projectile
;; ===============================================================================

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map) ;; map to "super" (command) key
              ("C-c p" . projectile-command-map)))

;; configure python custom project type for testing with docker-compose

(projectile-register-project-type 'python '("pytest.ini" "docker-compose.yaml")
                                  :project-file "pytest.ini"
                                  :test "docker-compose run test-watch"
                                  :test-prefix "test")

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
  :init (global-flycheck-mode)
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

;; (use-package python-pytest
;;  :straight t)

;; ===============================================================================
;; configure Docker
;; ===============================================================================

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

;;(use-package lsp-mode
;;  :straight t)

;; ===============================================================================
;; Configure counsel, ivy, swiper
;; ===============================================================================

(defun enable-ivy ()
  (use-package counsel
    :straight t
    :config
    (use-package ivy-rich
      :straight t
      :config
      (ivy-rich-mode 1)
      (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))
    (ido-mode 0)
    (ivy-mode 1)
    (setq ivy-wrap t)
    (global-set-key (kbd "C-s") 'swiper-isearch)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))))
;;(enable-ivy)

;; ===============================================================================
;; Configure Vertico and Orderless
;; ===============================================================================

(defun enable-vertico ()
  (use-package vertico
    :straight t
    :init
    (vertico-mode)
    (setq vertico-cycle t))

  (use-package consult
    :straight t
    :bind (
	   ("C-x b" . consult-buffer)
	   ("C-s" . consult-line))
    :config
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source))

  (use-package consult-projectile
    :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

  (use-package marginalia
    :straight t
    :bind (
	   :map minibuffer-local-map
		("M-A" . marginalia-cycle))
    :init
    (marginalia-mode))

  (recentf-mode 1)

  (use-package embark
    :straight t
    :bind
    (("C-." . embark-act)) ;; @TODO this is overwritten by evil mode
    :init
    (setq prefix-help-command #'embark-prefix-help-command)
    :config
    (add-to-list 'display-buffer-alist
		 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		   nill
		   (window-parameters (mode-line-format . none)))))

  (use-package embark-consult
    :straight t
    :after (embark consult)
    :demand t
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))
  
  (use-package savehist
    :straight t
    :init
    (savehist-mode))
  
  (use-package orderless
    :straight t
    :init
    (setq completion-styles '(orderless basic)
          comletion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion))))))
(enable-vertico)

;; ===============================================================================
;; configure evil mode
;; ===============================================================================

(use-package evil
  :straight t
  :config
  (use-package evil-surround
    :straight t
    :config
    (global-evil-surround-mode 1))
  (use-package evil-matchit
    :straight t
    :config
    (global-evil-matchit-mode 1))
  (evil-mode 1)
  (add-hook 'org-capture-mode-hook 'evil-insert-state) ;; use insert by default for org capture
  (add-hook 'git-commit-mode-hook 'evil-insert-state) ;; use insert mode by default for magit commits
  (when (dirvish-override-dired-mode))
    (evil-set-initial-state 'dired-mode 'emacs)
  (use-package goto-chg
    :straight t))


			      
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "1aa4243143f6c9f2a51ff173221f4fd23a1719f4194df6cef8878e75d349613d" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "a3010c151dc4f42d56dec26a85ae5640afc227bece71d058e394667718b66a49" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "3ee898efcd3fa5b63c4f15e225f3616497010f2347a514490be8b563edbd39d9" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" "c335adbb7d7cb79bc34de77a16e12d28e6b927115b992bccc109fb752a365c72" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "a9318f38c2d39f717d61aa0c155f579fc3a369c4a0d01f4848de0dee85fbd831" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" default))
 '(package-selected-packages
   '(blacken py-autopep8 flycheck elpy better-defaults material-theme vs-light-theme monokai-theme solarized-theme magit)))

