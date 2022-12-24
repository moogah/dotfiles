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
