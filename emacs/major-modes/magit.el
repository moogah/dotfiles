
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

(use-package forge
  :straight t
  :after magit
  :config
  (push '("github.csnzoo.com" "github.csnzoo.com/api/v3"
          "github.csnzoo.com" forge-github-repository)
        forge-alist))
