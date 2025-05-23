;; -*- lexical-binding: t; -*-

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map) ;; map to "super" (command) key
              ("C-c p" . projectile-command-map)))

;; configure python custom project type for testing with docker-compose
(projectile-register-project-type 'python '("pytest.ini" "docker-compose.yaml")
                                  :project-file "pytest.ini"
                                  :test "docker-compose run test-watch"
                                  :test-prefix "test")
