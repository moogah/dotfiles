
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
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package browse-at-remote
   :straight t
   :config)
   ;(add-to-list 'browse-at-remote-remote-type-regexps '("^github\\.csnzoo\\.com$" . "github")))

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


;; (use-package forge
;;   :straight t
;;   :after magit
;;   :config
;;   (push '("github.csnzoo.com" "github.csnzoo.com/api/v3"
;;           "github.csnzoo.com" forge-github-repository)
;;         forge-alist))
