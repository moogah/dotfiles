(defun create-tags-git-hook ()
  "Append ctags update command to git post-commit hook."
  (let* ((project-root (projectile-project-root))
         (hook-path (concat project-root ".git/hooks/post-commit"))
         (shebang "#!/bin/sh")
         (command "\n# auto-generated by emacs create-tags-git-hook\nctags -e -R .\n"))
    (when project-root
      (if (file-exists-p hook-path)
          (let ((existing-content (with-temp-buffer
                                    (insert-file-contents hook-path)
                                    (buffer-string))))
            (unless (string-match-p (regexp-quote command) existing-content)
              (append-to-file command nil hook-path)))
        (with-temp-file hook-path
          (insert shebang command))
        (set-file-modes hook-path #o755)))))

  (defadvice projectile-switch-project-by-name (after create-git-hook activate)
    "Create a git post-commit hook to update ctags every time a project is opened."
    (create-tags-git-hook))

  (defadvice projectile-switch-open-project (after create-git-hook activate)
    "Create a git post-commit hook to update ctags every time a project is opened."
    (create-tags-git-hook))

  (defadvice projectile-switch-project (after create-git-hook activate)
    "Create a git post-commit hook to update ctags every time a project is opened."
    (create-tags-git-hook))

  (defadvice consult-projectile-switch-project (after create-git-hook activate)
    "Create a git post-commit hook to update ctags every time a project is opened."
    (create-tags-git-hook))

(use-package ggtags
    :straight t)

  (add-hook 'python-mode-hook
          (lambda ()
            (ggtags-mode 1)))

(add-hook 'js-mode-hook
          (lambda ()
            (ggtags-mode 1)))

(setenv "GTAGSLABEL" "pygments")

; this config cargo culted from https://www.emacswiki.org/emacs/GnuGlobal


(defun gtags-root-dir ()
    "Returns GTAGS root directory or nil if doesn't exist."
    (with-temp-buffer
      (if (zerop (call-process "global" nil t nil "-pr"))
          (buffer-substring (point-min) (1- (point-max)))
        nil)))

  (defun gtags-update ()
    "Make GTAGS incremental update"
    (call-process "global" nil nil nil "-u"))

  (defun gtags-update-hook ()
    (when (gtags-root-dir)
      (gtags-update)))

  (add-hook 'after-save-hook #'gtags-update-hook)

;; ===============================================================================
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Development Configuration
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ===============================================================================

;; Configure common modes like yaml, json etc
(load "~/src/dotfiles/emacs/language-modes/yaml.el")
(load "~/src/dotfiles/emacs/language-modes/json.el")


(use-package wgrep
  :straight t)

;; ===============================================================================
;; Configure Tree Sitter
;; ===============================================================================

(load "~/src/dotfiles/emacs/language-modes/tree-sitter.el")

;; ===============================================================================
;; Configure markdown mode
;; ===============================================================================

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; ===============================================================================
;; Configure Yasnippet
;; ===============================================================================

(load "~/src/dotfiles/emacs/language-modes/yasnippet.el")

;; ===============================================================================
;; Configure Projectile
;; ===============================================================================

(load "~/src/dotfiles/emacs/language-modes/projectile.el")

;; ===============================================================================
;; python development config
;; ===============================================================================

(load "~/src/dotfiles/emacs/language-modes/python.el")

;; (use-package python-pytest
;;  :straight t)

;; ===============================================================================
;; configure Docker
;; ===============================================================================

(load "~/src/dotfiles/emacs/language-modes/docker.el")

;; ===============================================================================
;; Configure Ansible
;; ===============================================================================

;; ansible uses the yaml-mode configured in the general section

;; ===============================================================================
;; Configure Terraform
;; ===============================================================================

(use-package terraform-mode
  :straight t
  :config
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

;; ===============================================================================
;; Configure Puppet
;; ===============================================================================

(use-package puppet-mode
  :straight t)

;; ===============================================================================
;; Configure Golang
;; ===============================================================================

(load "~/src/dotfiles/emacs/language-modes/golang.el")

;; ===============================================================================
;; Configure GitHub Copilot
;; ===============================================================================

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))
  
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-j") 'copilot-accept-completion-by-line)
(define-key copilot-completion-map (kbd "C-l") 'copilot-accept-completionb-word)

;; a potentially useful blog with examples of restricting where/when copilot makes suggestions
;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
;; enable with ie: (add-hook 'prog-mode-hook 'copilot-mode)
