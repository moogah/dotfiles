(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package blacken
  :straight t
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

(add-hook 'python-mode-hook 'tree-sitter-hl-mode)
