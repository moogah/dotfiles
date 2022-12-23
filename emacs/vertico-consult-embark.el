(use-package perspective
  :straight t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (persp-mode))


;; ===============================================================================
;; Configure Company Auto-Completion
;; ===============================================================================

(use-package company
  :straight t
  :bind(:map company-active-map
             ([return] . nil)
             ("RET" . nil))
  :config
  (company-mode)
  (add-hook 'after-init-hook 'global-company-mode))


;; ===============================================================================
;; Configure Vertico and Orderless
;; ===============================================================================

(defun enable-vertico ()
  (use-package vertico
    :straight t
    :init
    (vertico-mode)
    (setq vertico-cycle t))

  (use-package consult
    :straight t
    :bind (
	   ("C-x b" . consult-buffer)
	   ("C-s" . consult-line))
    :config
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source))

  (use-package consult-projectile
    :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master"))

  (use-package marginalia
    :straight t
    :bind (
	   :map minibuffer-local-map
		("M-A" . marginalia-cycle))
    :init
    (marginalia-mode))

  (recentf-mode 1)

  (use-package embark
    :straight t
    :bind
    (("C-." . embark-act)) ;; @TODO this is overwritten by evil mode
    :init
    (setq prefix-help-command #'embark-prefix-help-command)
    :config
    (add-to-list 'display-buffer-alist
		 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		   nill
		   (window-parameters (mode-line-format . none)))))

  (use-package embark-consult
    :straight t
    :after (embark consult)
    :demand t
    :hook
    (embark-collect-mode . consult-preview-at-point-mode))
  
  (use-package savehist
    :straight t
    :init
    (savehist-mode))
  
  (use-package orderless
    :straight t
    :init
    (setq completion-styles '(orderless basic)
          comletion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion))))))
(enable-vertico)
