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

;; ===============================================================================
;; Evil Collection provides vim-like bindings for many emacs modes
;; ===============================================================================

(use-package evil-collection
  :straight t
  :after evil)

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

;; Core/global bindings (applied immediately)

;; Basic global commands
(evil-define-key 'normal 'global (kbd "<SPC> x") 'kill-this-buffer)
(evil-define-key 'normal 'global (kbd "<SPC> d") 'dired-jump)

;; Define prefix key for window commands
(define-prefix-command 'my-window-command-map)
(evil-define-key 'normal 'global (kbd "<SPC> w") 'my-window-command-map)
(define-key my-window-command-map (kbd "c") 'delete-window)
(define-key my-window-command-map (kbd "v") 'split-window-vertically)
(define-key my-window-command-map (kbd "j") 'evil-window-down)
(define-key my-window-command-map (kbd "k") 'evil-window-up)
(define-key my-window-command-map (kbd "h") 'my-split-or-switch-window-left)
(define-key my-window-command-map (kbd "l") 'my-split-or-switch-window-right)

;; Define prefix key for space prefix commands
(define-prefix-command 'my-space-command-map)
(evil-define-key 'normal 'global (kbd "<SPC> <SPC>") 'my-space-command-map)
(define-key my-space-command-map (kbd "j") 'previous-buffer)
(define-key my-space-command-map (kbd "k") 'next-buffer)

;; Hydra bindings
(with-eval-after-load 'hydra
  (define-key my-space-command-map (kbd "h") 'hydra-main/body))

;; Org mode bindings
(with-eval-after-load 'org
  ;; Global org bindings
  (evil-define-key 'normal 'global (kbd "<SPC> e") 'org-babel-execute-src-block)
  (evil-define-key 'normal 'global (kbd "<SPC> s l") 'org-store-link)
  (evil-define-key 'normal 'global (kbd "<SPC> i l") 'org-insert-link)

  ;; Org mode map specific bindings
  (evil-define-key 'normal org-mode-map (kbd "<SPC> h") 'org-insert-heading)
  (evil-define-key 'normal org-mode-map (kbd "<SPC> H") 'org-insert-subheading))

;; Org-roam bindings
(with-eval-after-load 'org-roam
  (evil-define-key 'normal 'global (kbd "<SPC> n") 'org-roam-node-find)
  (evil-define-key 'normal 'global (kbd "<SPC> j") 'org-roam-dailies-goto-today))

;; Projectile bindings
(with-eval-after-load 'projectile
  ;; Global projectile bindings
  (evil-define-key 'normal 'global (kbd "<SPC> r") 'projectile-ripgrep)
  (evil-define-key 'normal 'global (kbd "<SPC> f") 'project-find-file)

  ;; Define prefix key for projectile commands
  (define-prefix-command 'my-projectile-command-map)
  (evil-define-key 'normal 'global (kbd "<SPC> p") 'my-projectile-command-map)
  (define-key my-projectile-command-map (kbd "p") 'consult-projectile-switch-project)

  ;; Python mode specific bindings
  (evil-define-key 'normal python-mode-map (kbd "<SPC> T") 'my-find-implementation-or-test-other-window))

;; Magit bindings
(with-eval-after-load 'magit
  ;; Define prefix key for magit commands
  (define-prefix-command 'my-magit-command-map)
  (evil-define-key 'normal 'global (kbd "<SPC> g") 'magit)
  (evil-collection-init 'magit))

;; Consult bindings
(with-eval-after-load 'consult
  (evil-define-key 'normal 'global (kbd "<SPC> b") 'consult-bookmark)
  (evil-define-key 'normal 'global (kbd "<SPC> o") 'consult-buffer)

  ;; Define prefix key for menu commands
  (define-prefix-command 'my-menu-command-map)
  (evil-define-key 'normal 'global (kbd "<SPC> m") 'my-menu-command-map)
  (define-key my-menu-command-map (kbd "m") 'consult-imenu-multi)
  (define-key my-menu-command-map (kbd "i") 'consult-imenu))

;; Perspective bindings
(with-eval-after-load 'perspective
  ;; Add to projectile prefix map, creating it if needed
  (unless (fboundp 'my-projectile-command-map)
    (define-prefix-command 'my-projectile-command-map)
    (evil-define-key 'normal 'global (kbd "<SPC> p") 'my-projectile-command-map))

  (define-key my-projectile-command-map (kbd "s") 'persp-switch)
  (define-key my-projectile-command-map (kbd "S") 'persp-state-save)
  (define-key my-projectile-command-map (kbd "L") 'persp-state-load))

;; Tab-bar bindings
(with-eval-after-load 'tab-bar
  (evil-define-key 'normal 'global (kbd "<SPC> t") 'tab-switch))
