;; ===============================================================================
;; configure dired
;; ===============================================================================

(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-bookmark-entries
   '(("h" "~/"        "Home")
     ("s" "~/src/"    "Source Code")
     ("d" "~/src/dotfiles"    "Dotfiles")
     ("e" "~/.emacs.d/"    "Emacs Config")
     ("o" "~/org/"    "Org")
     ("l" "~/src/homelab/"    "Homelab")))
  (dirvish-mode-line-format
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (dirvish-attributes '(vscode-icon file-size collapse subtree-state vc-state))
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  (use-package vscode-icon
    :straight t
    :config
    (setq dirvish-vscode-icon-size 18)) ;; vs-code icons is an alternative
  ;;(dirvish-peek-mode)
  (setq dirvish-vscode-icon-size 18)
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  ;;(setq dired-mouse-drag-files t)                   ; added in emacs 29
  ;;(setq mouse-drag-and-drop-region-cross-program t) ; added in emacs 29
  (setq dired-listed-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (setf dired-kill-when-opening-new-dired-buffer t) ;; https://stackoverflow.com/questions/1839313/how-do-i-stop-emacs-dired-mode-from-opening-so-many-buffers
  :bind
  (("C-x d" . dired-jump)
   ("C-c f" . dirvish-fd)
   :map dirvish-mode-map
   ("h"   . dired-up-directory)
   ("j"   . dired-next-line)
   ("k"   . dired-previous-line)
   ("l"   . dired-find-file)
   ("i"   . wdired-change-to-wdired-mode)
   ("."   . dired-omit-mode)
   ("b"   . dirvish-bookmark-jump)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("H"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("TAB" . dirvish-subtree-toggle)
   ("M-n" . dirvish-history-go-forward)
   ("M-p" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(put 'dired-find-alternate-file 'disabled nil)

;; automatically update dired buffers when state-on-disk changes
(add-hook 'dired-mode-hook 'auto-revert-mode)
