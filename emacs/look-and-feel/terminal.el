;; -*- lexical-binding: t; -*-

;; Configure compilation output to handle color terminal output
(use-package xterm-color
  :straight t
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter))
