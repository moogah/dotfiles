;; -*- lexical-binding: t; -*-
;; Machine-specific configuration for Mac.home

;; Configure hostname-based daily journal directory
(setq org-roam-dailies-directory "dailies/Mac.home/")

(setq browser-hist-db-paths
      '((chrome . "/Users/jefffarr/Library/Application Support/Google/Chrome/Profile 1/History")))

(setq browser-hist-default-browser 'chrome)

;; PostgreSQL connections - sensitive details stored in ~/.authinfo.gpg
;; Only connection names and Docker-specific metadata are here.
;; After updating connections, run: M-x jf/postgres-register-connections

(setq jf/postgres-connections
      '((pg-local-apollo-container
         :docker-name "postgres")
        (pg-devnew-rw)))
