;; -*- lexical-binding: t; -*-

(toggle-debug-on-error)
;; use C-x r j i to quick jump to this file
(set-register ?i (cons 'file "~/src/dotfiles/emacs/init.el"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)

;; install use-package
(straight-use-package 'use-package)

;; attempt to force using straight for org
(straight-use-package 'org)

;; ===============================================================================
;; OSX Specific Configs
;; ===============================================================================

;; make sure we have access to the same PATH as in our zsh
(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

;; ===============================================================================
;; Global Dependencies
;; ===============================================================================

;; needed for projectile-ripgrep
(use-package ripgrep
  :straight t)

;; ===============================================================================
;; configure EasyPG
;; ===============================================================================

(setq auth-sources '("~/.authinfo.gpg"))
(setq epa-file-select-keys nil)
(setq epa-file-encrypt-to '("moogah@gmail.com"))

;; Fix EasyPG error.
;; From https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html.
;; .. from https://www.bytedude.com/gpg-in-emacs/
(defvar epa-pinentry-mode)
(setq epa-pinentry-mode 'loopback)

;; ===============================================================================
;; configure Global Behaviors
;; ===============================================================================

(use-package expand-region
  :straight t)

(use-package avy
  :straight t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; ==============================================================================
;; configure Window Management
;; ===============================================================================

(winner-mode 1)

(use-package perspective
  :straight t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (persp-mode))

(use-package ace-window
  :straight t)

(use-package tab-bookmark
  :straight (:host github :repo "minad/tab-bookmark" :branch "main" :files ("*.el")))

;; ===============================================================================
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; Org Mode Configuration
;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; ===============================================================================

(use-package ob-async
  :straight t
  :config)
  ;(setq ob-async-no-async-languages-alist '("ipython")))

;; ===============================================================================
;; Install dogears
;; ===============================================================================

(use-package dogears
  :straight t
  :config
  (dogears-mode t))

;; Jira Export
;;(use-package ox-jira
;;  :straight t
;;  :config
;;  (setq org-export-copy-to-kill-ring 'if-interactive))

;; ===============================================================================
;; Configure PDF Tools
;; ===============================================================================

;; (use-package pdf-tools
;;   :straight t; (pdf-tools :type git :host github :repo "vedang/pdf-tools")
;;   :config
;;   (setenv "PKG_CONFIG_PATH" "${PKG_CONFIG_PATH}:/opt/homebrew/bin/pkg-config:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig")
;;   (pdf-loader-install))

(load "~/src/dotfiles/emacs/look-and-feel/look-and-feel.el")
(load "~/src/dotfiles/emacs/major-modes/dirvish.el")
(load "~/src/dotfiles/emacs/major-modes/magit.el")
(load "~/src/dotfiles/emacs/major-modes/org.el")
(load "~/src/dotfiles/emacs/major-modes/org-roam.el")
(load "~/src/dotfiles/emacs/language-modes/ide-features.el")
(load "~/src/dotfiles/emacs/evil.el")
(load "~/src/dotfiles/emacs/hydra.el")
(load "~/src/dotfiles/emacs/elfeed.el")
(load "~/src/dotfiles/emacs/vertico-consult-embark.el")
(load "~/src/dotfiles/emacs/gpt.el") 
(if (string-equal system-name "ALT02886")
    (load "~/src/dotfiles/emacs/wayfair.el"))
;; @TODO some packages have config lines which depend on prior install ie: dired and evil
;; to load an entire directory
;; (setq load-path (cons "~/emacs" load-path))




;; ===============================================================================
;; Experimental Packages
;; ===============================================================================

(use-package sqlite
  :straight t)

(use-package browser-hist
  :straight (browser-hist :type git :host github :repo "agzam/browser-hist.el")
  :config
  (setq browser-hist-db-paths
        '((chrome . "/Users/jefffarr/Library/Application Support/Google/Chrome/Default/History")
          (arc . "/Users/jefffarr/Library/Application Support/Arc/User Data/Default/History")))
 (setq browser-hist-default-browser 'chrome))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "5b9a45080feaedc7820894ebbfe4f8251e13b66654ac4394cb416fef9fdca789" "b9761a2e568bee658e0ff723dd620d844172943eb5ec4053e2b199c59e0bcc22" "f053f92735d6d238461da8512b9c071a5ce3b9d972501f7a5e6682a90bf29725" "1aa4243143f6c9f2a51ff173221f4fd23a1719f4194df6cef8878e75d349613d" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "6945dadc749ac5cbd47012cad836f92aea9ebec9f504d32fe89a956260773ca4" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "683b3fe1689da78a4e64d3ddfce90f2c19eb2d8ab1bab1738a63d8263119c3f4" "a3010c151dc4f42d56dec26a85ae5640afc227bece71d058e394667718b66a49" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "991ca4dbb23cab4f45c1463c187ac80de9e6a718edc8640003892a2523cb6259" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "3ee898efcd3fa5b63c4f15e225f3616497010f2347a514490be8b563edbd39d9" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" "c335adbb7d7cb79bc34de77a16e12d28e6b927115b992bccc109fb752a365c72" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "a9318f38c2d39f717d61aa0c155f579fc3a369c4a0d01f4848de0dee85fbd831" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" default))
 '(evil-respect-visual-line-mode t)
 '(package-selected-packages
   '(blacken py-autopep8 flycheck elpy better-defaults material-theme vs-light-theme monokai-theme solarized-theme magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.75))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.5))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
