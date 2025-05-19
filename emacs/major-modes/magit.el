﻿;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Core Magit Setup
;; ===============================================================================

(use-package magit
  :straight t
  :config
  ;; Use full-frame status display
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; Restore window configuration when quitting magit
  (setq magit-bury-buffer-function 'magit-restore-window-configuration))

;; ===============================================================================
;; GitHub Integration - Browse At Remote
;; ===============================================================================

(use-package browse-at-remote
   :straight t)

;; ===============================================================================
;; GitHub Pull Request Integration
;; ===============================================================================

;; Create a URL to open a new pull request for current branch
;; From: https://emacs.stackexchange.com/questions/3900/command-to-visit-github-pull-request-of-current-branch-with-magit
(defun new-pull-request-url ()
  "Build the URL for a new pull request on GitHub for the current branch.
Uses Magit to get the remote URL and current branch."
  (interactive)
  ;; alternately use %s/compare/%s to simply get a comparison
  (format "%s/pull/new/%s"
           (replace-regexp-in-string
            (rx (and string-start (1+ any) "github.com:" (group (1+ any)) ".git" string-end))
            "https://github.com/\\1"
            (magit-get "remote" (magit-get-current-remote) "url"))
          (magit-get-current-branch)))

;; To use this: (browse-url (new-pull-request-url))

;; For future consideration:
;; (use-package forge
;;   :straight t
;;   :after magit)

;; ===============================================================================
;; Magit Window Management
;; ===============================================================================

;; Save and restore window configuration when using Magit
;; Note: This functionality may be redundant with current magit-bury-buffer-function
(defun my-magit-status ()
  "Open Magit status while saving current window configuration."
  (interactive)
  (window-configuration-to-register ?m)
  (magit-status)
  (add-hook 'magit-quit-hook
            (lambda ()
              (message "Restoring window layout from register m...")
              (jump-to-register ?m)
              (remove-hook 'magit-quit-hook (nth 0 magit-quit-hook)))
            nil t))
