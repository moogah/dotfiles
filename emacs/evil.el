;; ===============================================================================
;; configure evil mode
;; ===============================================================================

(use-package evil
  :straight t
  :config
  (use-package evil-surround
    :straight t
    :config
    (global-evil-surround-mode 1))
  (use-package evil-matchit
    :straight t
    :config
    (global-evil-matchit-mode 1))
  (evil-mode 1)
  (setq evil-want-fine-undo t)
  (add-hook 'org-capture-mode-hook 'evil-insert-state) ;; use insert by default for org capture
  (add-hook 'git-commit-mode-hook 'evil-insert-state) ;; use insert mode by default for magit commits
  (when (dirvish-override-dired-mode))
    (evil-set-initial-state 'dired-mode 'emacs)
  (use-package goto-chg
    :straight t))

;; add visual indicators for common vim commands
(use-package evil-goggles
  :straight t
  :config
  (evil-goggles-mode))

(use-package evil-args
  :straight t
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-normal-state-map "L" 'evil-forward-arg)
  (define-key evil-normal-state-map "H" 'evil-backward-arg)
  (define-key evil-motion-state-map "L" 'evil-forward-arg)
  (define-key evil-motion-state-map "H" 'evil-backward-arg)
  (define-key evil-normal-state-map "K" 'evil-jump-out-args))