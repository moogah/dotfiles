(use-package emacsql-sqlite
  :straight (emacsql-sqlite :type git :host github :repo "magit/emacsql" :branch main :files ("emacsql-sqlite.el" "emacsql-sqlite-common.el" "sqlite")))

;; ===============================================================================
;; Configure Org-Roam
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

(setq org-roam-dailies-directory "dailies/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" plain
         (file "~/.emacs.d/templates/org-roam-dailies-default.org")
         :if-new (file+head "apploi-%<%Y-%m-%d>.org"
                            "#+title: apploi-%<%Y-%m-%d>\n\n* %<%Y-%m-%d> Meetings\n\n* %<%Y-%m-%d> Worklog\n\n* %<%Y-%m-%d> Tasks")
         :unnarrowed t)))

(require 'org-roam-protocol)

(server-start)

(setq org-roam-capture-templates
 '(("d" "default" plain
    "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
    :unnarrowed t)
   ("f" "foo" plain
    (file "~/.emacs.d/templates/org-roam-default.org")
    :if-new (file+head "inbox/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n ${body}")
    :unnarrowed t)))

(setq org-roam-capture-ref-templates
      '(("r" "ref" plain
        "%?"
        :target
        (file+head "${slug}.org" "#+title: ${title}\n${body}")
        :unnarrowed t)))
