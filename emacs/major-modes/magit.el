﻿;; ===============================================================================
;; install magit
;; ===============================================================================

(use-package magit
  :straight t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration))

;; ===============================================================================
;; install browse-at-remote
;; ===============================================================================

(use-package browse-at-remote
   :straight t
   :config)

;; ===============================================================================
;; magit customizations
;; ===============================================================================

; Saving and restoring the window layout work, but the magit-quit-hook part doesn't seem to do it
; also this is apparently what the default behavior is supposed to be
(defun my-magit-status ()
  (interactive)
  (window-configuration-to-register ?m)
  (magit-status)
  (add-hook 'magit-quit-hook
            (lambda ()
              (message "Restoring window layout from register m...")
              (jump-to-register ?m)
              (remove-hook 'magit-quit-hook (nth 0 magit-quit-hook)))
            nil t))

; two different approaches to navigating to the current branches pull request
; cargo culted from https://emacs.stackexchange.com/questions/3900/command-to-visit-github-pull-request-of-current-branch-with-magit?newreg=420aae794802477e8d03a30e5af21492
(defun new-pull-request-url ()
  "Build the URL or the pull requestion on GitHub corresponding
to the current branch. Uses Magit."
  (interactive)
  ;; alternately use %s/compare/%s to simply get a comparison
  (format "%s/pull/new/%s"
           (replace-regexp-in-string
            (rx (and string-start (1+ any) "github.com:" (group (1+ any)) ".git" string-end))
            "https://github.com/\\1"
            (magit-get "remote" (magit-get-current-remote) "url"))
          (magit-get-current-branch)))

;; this works!
;; (browse-url (pull-request-url))

;; (use-package forge
;;   :straight t
;;   :after magit)

;; ===============================================================================
;; Evil-friendly Magit key bindings
;; ===============================================================================

;; Fix evil behavior in magit buffers
(use-package evil-collection
  :straight t
  :after magit
  :config
  ;; Enable full evil integration with magit
  (evil-collection-init 'magit))
