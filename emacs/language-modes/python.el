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

(use-package blacken
  :straight t)

(defun setup-python-evil-word ()
  "Configure evil-words in python-mode."
  (modify-syntax-entry ?_ "w" python-mode-syntax-table))

(add-hook 'python-mode-hook 'setup-python-evil-word)
