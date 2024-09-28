
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

;; enable mouse horizontal scrolling
;; use toggle-truncate-lines or visual-line-mode as alternatives
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)
(set-default 'truncate-lines t) ;; don't wrap lines

;; install adaptive-wrap so that visual-line-mode will respect local indentation levels
(use-package adaptive-wrap
  :straight t
  :config
  (setq adaptive-wrap-extra-indent 2) ;; set evil respect visual line mode
  (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)) ;; (lambda () (setq adaptive-wrap-prefix-mode t))))

(global-visual-line-mode 1)
(setq word-wrap 1)

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

;; Display line numbers in the left margin
(global-display-line-numbers-mode)
;; set to 'relative to display the count distance from the current line
(setq display-line-numbers-type 't)

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
(use-package timu-macos-theme
  :straight t)
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
;;(set-background-color "black")

;; https://emacs.stackexchange.com/questions/48365/custom-theme-set-faces-does-not-work-in-emacs-27
(custom-theme-set-faces
 'doom-palenight
 '(default ((t (:background "black"))))
 '(font-lock-comment-face ((t (:foreground "dark gray"))))
 '(tree-sitter-hl-face:method.call ((t (:foreground "light blue"))))
 '(tree-sitter-hl-face:function.call ((t (:foreground "light blue"))))
 '(tree-sitter-hl-face:type ((t (:foreground "DodgerBlue1"))))
 '(magit-diff-hunk-heading ((t (:background "black" :foreground "dark gray"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "black" :foreground "wheat1"))))
 '(magit-diff-context-highlight ((t (:background "black"))))
 '(magit-diff-removed-highlight ((t (:background "black" :foreground "red"))))
 '(magit-diff-removed ((t (:background "#292D3E" :foreground "#cc4259"))))
 '(magit-diff-added-highlight ((t (:background "black" :foreground "green"))))
 '(magit-diff-added ((t (:background "#292D3E" :foreground "#9cb970"))))
 '(org-table ((t (:background "#292D3E" :foreground "wheat1"))))
 '(org-default ((t (:foreground "wheat1"))))
 '(org-link ((t (:foreground "DodgerBlue1"))))
 '(org-level-1 ((t (:foreground "white"))))
 '(org-level-2 ((t (:foreground "white"))))
 '(org-level-3 ((t (:foreground "white"))))
 '(org-level-4 ((t (:foreground "white"))))
 '(org-level-5 ((t (:foreground "white"))))
 '(font-lock-string-face ((t (:foreground "wheat1")))))

(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars t)
(setq org-startup-indented t)

;; this setting will break capture templates 
;;(setq org-blank-before-new-entry nil)
;;

(enable-theme 'doom-palenight)

;;(add-hook 'org-mode-hook 'visual-line-mode)

;; set font size to 14pt for my aging eyes
(setq default-frame-alist '((font . "Menlo-14")))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.75))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
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
