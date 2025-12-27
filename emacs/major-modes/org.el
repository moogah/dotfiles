;; -*- lexical-binding: t; -*-

;; Ensure we have a consistent org version
;; This is needed for compatibility with org-roam and other packages
(straight-use-package 'org)

;; ===============================================================================
;; Core Org Mode Settings
;; ===============================================================================

;; Source code blocks settings
(setq org-src-tab-acts-natively t)
(setq org-src-fontify-natively t)

;; Configure Babel languages
(org-babel-do-load-languages 'org-babel-load-languages
                             (append org-babel-load-languages
                                     '((python . t)
                                       (emacs-lisp . t)
                                       (js . t)
                                       (sql . t)
                                       (shell . t))))

;; Python specific settings
(setq org-babel-python-command "python3 2>&1")

;; Wrapping and line handling
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-startup-truncated nil)

;; Enable horizontal scrolling
(use-package org-phscroll
  :straight '(org-phscroll :type git :host github :repo "misohena/phscroll"))

(with-eval-after-load "org"
  (require 'org-phscroll))

;; Modern appearance for org mode
(use-package org-modern
  :straight t)

;; Replace asterisks with bullets
(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Improved margins in Org mode
;; https://github.com/rougier/org-margin
(use-package org-margin
 :straight (org-margin :type git :host github :repo "rougier/org-margin"))

;; TODO add adaptive-wrap-prefix-mode

;; ===============================================================================
;; Configure Org Crypt
;; ===============================================================================

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

;; ===============================================================================
;; Org Export Engine Config
;; ===============================================================================

;; Jira Export
(use-package ox-jira
  :straight t
  :config
  (setq org-export-copy-to-kill-ring 'if-interactive))

;; Priority range and defaults
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;; Priority appearance
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;; Agenda key binding
(global-set-key (kbd "C-c a") 'org-agenda)

;; Agenda files
(setq org-agenda-files '("~/org/" 
                          "~/org/agenda" 
                          "~/org/roam/" 
                          "~/org/roam/inbox/" 
                          "~/org/roam/dailies"))

;; Display sorting for TODO items

(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down category-up)
        (todo priority-down alpha-up)
        (tags priority-down category-up alpha-up)
        (search category-up)))

;; Open agenda in current window
(setq open-agenda-window-setup (quote current-window))

;; Custom agenda views
;; From https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High Priority Tasks:")))
          (agenda "")
          (alltodo "")))))

;; Capture key binding
(define-key global-map (kbd "C-c c") 'org-capture)

;; Basic capture templates
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/todo.org" "Tasks")
         "* TODO [#B] %?" :empty-lines-before 1)))

;; Git integration for Org
(use-package orgit
  :straight (orgit :type git :host github :repo "magit/orgit"))

;; ===============================================================================
;; Setup auto-tangle for org files
;; ===============================================================================

(use-package org-auto-tangle
  :straight t
  :hook (org-mode . org-auto-tangle-mode))
;; enable in a doc with #+auto_tangle: t
;; enable in all buffers with org-auto-tangle-default

;; ===============================================================================
;; Setup Org Transclude
;; ===============================================================================

(use-package org-transclusion
  :straight t
  :after org)

;; ===============================================================================
;; Install corg for org-babel and dynamic block completions
;; ===============================================================================
(use-package corg
  :straight (:host github :repo "isamert/corg.el"))

;; ===============================================================================
;; Install org noter for annotating pdf files
;; ===============================================================================

(use-package org-noter
  :straight t)

;; ===============================================================================
;; Install org-download
;; ===============================================================================

(use-package org-download
  :straight t)

;; ===============================================================================
;; Install ob-async for async babel execution
;; ===============================================================================

(use-package ob-async
  :straight t
  :config)
  ;; Uncomment to disable async for specific languages
  ;(setq ob-async-no-async-languages-alist '("ipython"))
