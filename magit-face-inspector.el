;;; magit-face-inspector.el --- Inspect magit face definitions

(defun inspect-magit-faces ()
  "Inspect and report current magit face definitions."
  (interactive)
  (let ((faces '(magit-section-highlight
                 magit-diff-hunk-heading
                 magit-diff-hunk-heading-highlight
                 magit-diff-context-highlight
                 magit-diff-added
                 magit-diff-added-highlight
                 magit-diff-removed
                 magit-diff-removed-highlight))
        (output-buffer (get-buffer-create "*Magit Face Inspector*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert "=== MAGIT FACE INSPECTION REPORT ===\n\n")
      (insert (format "Emacs Version: %s\n" emacs-version))
      (insert (format "Display Type: %s\n" (if (display-graphic-p) "GUI" "Terminal")))
      (insert (format "Current Theme: %s\n\n" custom-enabled-themes))

      (dolist (face faces)
        (insert (format "--- %s ---\n" face))
        (if (facep face)
            (let* ((face-attrs (face-all-attributes face))
                   (background (face-attribute face :background nil 'default))
                   (foreground (face-attribute face :foreground nil 'default))
                   (inherit (face-attribute face :inherit nil 'default))
                   (face-spec (get face 'theme-face))
                   (face-customization (get face 'customized-face)))
              (insert (format "  Background: %s\n" background))
              (insert (format "  Foreground: %s\n" foreground))
              (insert (format "  Inherit: %s\n" inherit))
              (insert (format "  Defined in theme: %s\n" (if face-spec "YES" "NO")))
              (insert (format "  Customized: %s\n" (if face-customization "YES" "NO")))

              ;; Show where the face gets its actual colors from
              (let ((bg-source (face-attribute face :background nil t))
                    (fg-source (face-attribute face :foreground nil t)))
                (insert (format "  Effective background: %s\n" bg-source))
                (insert (format "  Effective foreground: %s\n" fg-source)))

              (insert "\n"))
          (insert "  Face not defined!\n\n")))

      (insert "\n=== RELATED FACES ===\n\n")
      (dolist (face '(hl-line region default))
        (insert (format "--- %s ---\n" face))
        (let ((background (face-attribute face :background nil 'default))
              (foreground (face-attribute face :foreground nil 'default)))
          (insert (format "  Background: %s\n" background))
          (insert (format "  Foreground: %s\n\n" foreground)))))

    (pop-to-buffer output-buffer)
    (special-mode)))

(provide 'magit-face-inspector)
;;; magit-face-inspector.el ends here
