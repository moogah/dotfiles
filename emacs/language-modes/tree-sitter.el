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
