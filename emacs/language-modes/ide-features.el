
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
