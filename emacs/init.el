(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Install Packages https://wikemacs.org/wiki/Package.el
(require 'cl-lib)

(defvar my-packages
  '(magit) "A list of packages to ensure are installed at launch")

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
	   when (not (package-installed-p p)) do (cl-return nil)
	   finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;; Configs from pragmaticemacs.wordpress.com Org-Mode TODO

;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; file to save todo items in
(setq org-agenda-files (quote ("~/todo.org")))

;; set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;; set colors for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
			   (?B . (:foreground "LightSteelBlue"))
			   (?C . (:foreground "OliveDrab"))))

;; open agenda in current window
(setq open-agenda-window-setup (quote current-window))

;; capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/todo.org" "Tasks")
	 "* TODO [#A] %?")))
			      
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
