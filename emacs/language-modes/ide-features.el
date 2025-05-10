;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Development Environment Configuration
;; ===============================================================================

;; Load GTags configuration
(load "~/src/dotfiles/emacs/language-modes/gtags.el")

;; Load Tree-sitter configuration
(load "~/src/dotfiles/emacs/language-modes/tree-sitter.el")

;; Configure common modes
(load "~/src/dotfiles/emacs/language-modes/yaml.el")
(load "~/src/dotfiles/emacs/language-modes/json.el")
(load "~/src/dotfiles/emacs/language-modes/markdown-mode.el")
(load "~/src/dotfiles/emacs/language-modes/terraform.el")
(load "~/src/dotfiles/emacs/language-modes/puppet-mode.el")

;; Load language-specific configurations
(load "~/src/dotfiles/emacs/language-modes/python.el")
(load "~/src/dotfiles/emacs/language-modes/typescript.el")
(load "~/src/dotfiles/emacs/language-modes/golang.el")
(load "~/src/dotfiles/emacs/language-modes/docker.el")

;; Load tool configurations
(load "~/src/dotfiles/emacs/language-modes/projectile.el")
(load "~/src/dotfiles/emacs/language-modes/yasnippet.el")
(load "~/src/dotfiles/emacs/language-modes/copilot.el")

(use-package wgrep
  :straight t)
