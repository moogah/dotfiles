(require 'mu4e)
(setq mu4e-maildir (expand-file-name "~/mu4e"))
(setq mu4e-sent-folder "/[Gmail]/Sent Mail")
(setq mu4e-drafts-folder "/[Gmail]/Drafts")
(setq mu4e-trash-folder "/[Gmail]/Trash")
(setq mu4e-refile-folder "/[Gmail]/All Mail")
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-update-interval (* 60 5))
(setq mu4e-compose-format-flowed t)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "moogah@gmail.com@gmail.com" nil))
      user-mail-address "moogah@gmail.com"
      user-full-name "Jeff Farr")
(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)
        ("/[Gmail]/Sent Mail" . ?s)
        ("/[Gmail]/Drafts" . ?d)
        ("/[Gmail]/Trash" . ?t)
        ("/[Gmail]/All Mail" . ?a)))
