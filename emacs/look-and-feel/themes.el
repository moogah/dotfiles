;; -*- lexical-binding: t; -*-

(use-package monokai-theme
  :straight t)
(use-package material-theme
  :straight t)
(use-package vs-light-theme
  :straight t)
(use-package color-theme-sanityinc-tomorrow
  :straight t)
(use-package cyberpunk-theme
  :straight t)
(use-package afternoon-theme
  :straight t)
(use-package darkburn-theme
  :straight t)
(use-package distinguished-theme
  :straight t)
(use-package doom-themes
  :straight t)
(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme"))
(use-package timu-macos-theme
  :straight t)

;; set default theme
;; doom-henna
;; doom-peacock
;; doom-vibrant
;; doom-ir
;; doom-old-hope
;; doom-ephemeral
;; doom-laserwave
;; doom-moonlight
;; doom-palenight
(load-theme 'doom-palenight t)
;; (load-theme 'distinguished)
;; (enable-theme 'distinguished)
;;(set-background-color "black")

;; set font size to 14pt for my aging eyes
;; Add font to existing default-frame-alist instead of replacing it
(add-to-list 'default-frame-alist '(font . "Menlo-14"))
