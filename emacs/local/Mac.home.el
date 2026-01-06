;; -*- lexical-binding: t; -*-
;; Machine-specific configuration for Mac.home

;; Configure hostname-based daily journal directory
(setq org-roam-dailies-directory "dailies/Mac.home/")

(setq browser-hist-db-paths
      '((chrome . "/Users/jefffarr/Library/Application Support/Google/Chrome/Profile 1/History")))

(setq browser-hist-default-browser 'chrome)

;; PostgreSQL connections for this machine
(setq jf/postgres-connections
      '((pg-local-apollo-container
         :host "localhost"
         :port 5432
         :database "postgres"
         :user "postgres"
         :auth-key "pg-local-apollo-container")
        (pg-devnew-rw
         :host "oncall-psql-devnew-20241205190430184400000002.cluster-cto8yse48viq.us-east-2.rds.amazonaws.com"
         :port 5432
         :database "oncall"
         :user "oc_admin_user"
         :auth-key "pg-devnew-rw")))
