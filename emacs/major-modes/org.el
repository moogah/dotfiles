
;; ===============================================================================
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Org Mode Configuration
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ===============================================================================
(setq org-src-tab-acts-natively t)
(setq org-src-fontify-natively t)
(org-babel-do-load-languages 'org-babel-load-languages
                             (append org-babel-load-languages
                                     '((python . t)
                                       (js . t)
                                       (shell . t))))
(setq org-babel-python-command "python3 2>&1")


;; Enable word wrapping in Org mode
(add-hook 'org-mode-hook 'visual-line-mode)

(use-package org-phscroll
  :straight '(org-phscroll :type git :host github :repo "misohena/phscroll")
  )

(setq org-startup-truncated nil)
(with-eval-after-load "org"
  (require 'org-phscroll))

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


;; ===============================================================================
;; Configs from pragmaticemacs.wordpress.com Org-Mode TODO
;; ===============================================================================

;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; file to save todo items in
(setq org-agenda-files '("~/org/" "~/org/agenda" "~/org/roam/" "~/org/roam/inbox/"))

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

;; ===============================================================================
;; Configure Org Agenda
;; ===============================================================================

;; lifted from https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High Priority Tasks:")))
          (agenda "")
          (alltodo "")))))


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
