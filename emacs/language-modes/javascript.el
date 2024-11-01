(use-package js2-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode)))

(use-package rjsx-mode
  :straight t
  :mode "\\.jsx\\'"
  :init
  (add-hook 'rjsx-mode-hook 'lsp))

(use-package prettier-js
  :straight t
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(defun my/use-eslint-from-node-modules ()
  "Use local eslint from node_modules before global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(defun my/use-prettier-from-node-modules ()
  "Use local prettier from node_modules before global."
  (let* ((project-root (or
                        (projectile-project-root)
                        (locate-dominating-file default-directory "node_modules")))
         (prettier (and project-root
                        (expand-file-name "node_modules/.bin/prettier"
                                          project-root))))
    (if (and prettier (file-executable-p prettier))
        (setq-local prettier-js-command prettier)
      (setq-local prettier-js-command "prettier"))))
(add-hook 'prettier-js-mode-hook #'my/use-prettier-from-node-modules)
