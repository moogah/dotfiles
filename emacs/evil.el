﻿;; ===============================================================================
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

(defun my-split-or-switch-window-left ()
  "Create a new window on the left and open dired, if a window already exists move there"
  (interactive)
  (if (= 1 (count-windows))
      (progn
        (split-window-horizontally)
        (dired-jump nil))
    (progn
      (evil-window-left 1))))


(defun my-split-or-switch-window-right ()
  "Create a new window on the right and open dired, if a window already exists move there"
  (interactive)
  (if (= 1 (count-windows))
      (progn
        (split-window-horizontally)
        (other-window 1)
        (dired-jump nil))
    (progn
      (evil-window-right 1))))

(defun my-find-implementation-or-test-other-window ()
  "Finds the corresponding test or implementation window and opens it in a new or existing horizontal split"
  (interactive)
  (let ((file (projectile-find-implementation-or-test (buffer-file-name))))
    (if file
        (progn (my-split-or-switch-window-right) (find-file file)))))

(evil-define-key 'normal 'global
  ;; Open Main Hydra Menu
  (kbd "<SPC> <SPC> h") 'hydra-main/body

  ;; Org Babel
  (kbd "<SPC> e") 'org-babel-execute-src-block

  ;; Org Roam
  (kbd "<SPC> n") 'org-roam-node-find
  (kbd "<SPC> j") 'org-roam-dailies-goto-today

  ;; Projectile
  (kbd "<SPC> r") 'projectile-ripgrep
  (kbd "<SPC> f") 'project-find-file

  ;; Magit
  (kbd "<SPC> g") 'magit

  ;; Dirvish
  (kbd "<SPC> d") 'dired-jump


  (kbd "<SPC> b") 'consult-bookmark

  ;; Perspective
  (kbd "<SPC> p s") 'persp-switch
  (kbd "<SPC> p S") 'persp-state-save
  (kbd "<SPC> p L") 'persp-state-load

  ;; Projectile
  (kbd "<SPC> p p") 'consult-projectile-switch-project


  ;; Buffers
  (kbd "<SPC> o") 'consult-buffer
  (kbd "<SPC> x") 'kill-this-buffer

  ;; Tabs
  (kbd "<SPC> t") 'tab-switch

  ; Links
  (kbd "<SPC> s l") 'org-store-link
  (kbd "<SPC> i l") 'org-insert-link

  ;; Window Management
  (kbd "<SPC> w c") 'delete-window
  (kbd "<SPC> w v") 'split-window-vertically
  (kbd "<SPC> w h") 'my-split-or-switch-window-left
  (kbd "<SPC> w j") 'evil-window-down
  (kbd "<SPC> w k") 'evil-window-up
  (kbd "<SPC> w l") 'my-split-or-switch-window-right

  ;; imenu
  (kbd "<SPC> m m") 'consult-imenu-multi
  (kbd "<SPC> m i") 'consult-imenu

  ;; buffer history
  (kbd "<SPC> <SPC> j") 'previous-buffer
  (kbd "<SPC> <SPC> k") 'next-buffer
)

(evil-define-key 'normal 'python-mode-map
  (kbd "<SPC> T") 'my-find-implementation-or-test-other-window)


(evil-define-key 'normal 'org-mode-map
  ; TODO
  ;(kbd "<SPC> t") 'org-insert-todo-heading
  ;(kbd "<SPC> T") 'org-insert-todo-subheading


  ; Headings
  (kbd "<SPC> h") 'org-insert-heading
  (kbd "<SPC> H") 'org-insert-subheading)
