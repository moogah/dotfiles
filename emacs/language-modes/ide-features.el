
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

(use-package tree-sitter
  :straight t
  :hook ((python-mode . tree-sitter-mode)
         (python-mode . tree-sitter-hl-mode)
         (js-mode . tree-sitter-mode)
         (js-mode . tree-sitter-hl-mode)
         (javascript-mode . tree-sitter-mode)
         (javascript-mode . tree-sitter-hl-mode)
         (sh-mode . tree-sitter-mode)
         (sh-mode . tree-sitter-hl-mode)
         (php-mode . tree-sitter-mode)
         (php-mode . tree-sitter-hl-mode)
         (hcl-mode . tree-sitter-mode)
         (hcl-mode . tree-sitter-hl-mode)
         (terraform-mode . tree-sitter-mode)
         (terraform-mode . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :straight t)

(use-package evil-textobj-tree-sitter
  :straight t)

(use-package combobulate
  :straight (combobulate :type git :host github :repo "mickeynp/combobulate")
  :hook ((python-mode . combobulate-mode)
         (js-mode . combobulate-mode)
         (typescript-mode . combobulate-mode)))

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

(use-package yasnippet
  :straight t
  :config
  (use-package yasnippet-snippets
    :straight t)
    (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"                                       ;; personal snippets
          "~/.emacs.d/straight/repos/yasnippet-snippets/snippets"))   ;; yasnippet snippets
  (yas-global-mode 1))

(use-package auto-yasnippet
  :straight t)
;; @TODO
;; bind aya-create and aya-expand

(use-package autoinsert
  :straight t
  :init
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'file-file-hook 'auto-insert)
  (auto-insert-mode 1))

(defun jf/autoinsert-yas-expand()
  "Replace text in yasnippet template."
  (interactive)
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(define-auto-insert "\\.pp$" ["default-puppet.pp" jf/autoinsert-yas-expand])

;; ===============================================================================
;; Configure Projectile
;; ===============================================================================

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map) ;; map to "super" (command) key
              ("C-c p" . projectile-command-map)))

;; configure python custom project type for testing with docker-compose

(projectile-register-project-type 'python '("pytest.ini" "docker-compose.yaml")
                                  :project-file "pytest.ini"
                                  :test "docker-compose run test-watch"
                                  :test-prefix "test")

(defvar docker-compose-cli-path
  "/usr/local/bin/docker-compose"
  "Path to the docker compose executable.")

;; ===============================================================================
;; python development config
;; ===============================================================================

(use-package elpy
  :straight t
  :config
  (elpy-enable)
  (setq elpy-modules (delq 'elpy-module-highlight-indentation elpy-modules)))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :config
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package py-autopep8
  :straight t
  :config
  (add-hook 'python-mode-hook 'py-autopep8-mode))

(use-package blacken
  :straight t)

;; (use-package python-pytest
;;  :straight t)

;; ===============================================================================
;; configure Docker
;; ===============================================================================

(use-package docker
  :straight t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :straight t)

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
