;; -*- lexical-binding: t; -*-

(use-package emacsql-sqlite
  :straight (emacsql-sqlite :type git :host github :repo "magit/emacsql" :branch main :files ("emacsql-sqlite.el" "emacsql-sqlite-common.el" "sqlite")))

;; ===============================================================================
;; Configure Org-Roam Core
;; ===============================================================================

(use-package org-roam
  :straight (org-roam :host github :repo "org-roam/org-roam" :tag "v2.2.2")
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n d" . org-roam-dailies-goto-today))
  :config
  (setq org-roam-directory (file-truename "~/org/roam"))
  (setq find-file-visit-truename t)
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode))

;; Set the directory for daily notes
(setq org-roam-dailies-directory "dailies/")

;; Configure daily note templates
(setq org-roam-dailies-capture-templates
      '(("d" "default" plain
         (file "~/.emacs.d/templates/org-roam-dailies-default.org")
         :if-new (file+head "apploi-%<%Y-%m-%d>.org"
                            "#+title: apploi-%<%Y-%m-%d>\n\n* %<%Y-%m-%d> Meetings\n\n* %<%Y-%m-%d> Worklog\n\n* %<%Y-%m-%d> Tasks")
         :unnarrowed t)))

;; Enable org-roam-protocol
(require 'org-roam-protocol) 

;; Ensure Emacs server is running (required for protocol)
(server-start)

;; Configure standard capture templates
(setq org-roam-capture-templates
 '(("d" "default" plain
    "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)
   ("f" "foo" plain
    (file "~/.emacs.d/templates/org-roam-default.org")
    :if-new (file+head "inbox/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n ${body}")
    :unnarrowed t)))

;; Templates for capturing web content
(setq org-roam-capture-ref-templates
      '(("r" "ref" plain
        "%?"
        :target
        (file+head "${slug}.org" "#+title: ${title}\n${body}")
        :unnarrowed t)))

;; Configure automatic Git synchronization
(use-package git-sync-mode
  :straight (git-sync-mode :host github :repo "justinbarclay/git-sync-mode")
  :config
  (git-sync-global-mode)
  (add-to-list 'git-sync-allow-list '"~/org/roam"))
