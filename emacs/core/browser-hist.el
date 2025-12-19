;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure browser-hist for accessing browser history
;; ===============================================================================

(use-package browser-hist
  :straight (browser-hist :type git :host github :repo "agzam/browser-hist.el")
  :config
  (setq browser-hist-db-paths
        '((chrome . "/Users/jefffarr/Library/Application Support/Google/Chrome/Default/History")
          (safari . "/Users/jefffarr/Library/Safari/History.db")))
  (setq browser-hist-default-browser 'safari))
