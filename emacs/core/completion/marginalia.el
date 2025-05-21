;; -*- lexical-binding: t; -*-

;; ===============================================================================
;; Configure Marginalia - Rich Annotations for Minibuffer Completions
;; ===============================================================================

(use-package marginalia
  :straight (marginalia :host github :repo "minad/consult/marginalia") ; later versions require emacs 29
  :bind (
   :map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))
