;;; mod-email.el --- Email application. -*- lexical-binding: t; -*-

;;; Commentary:
;; Emma uses mu4e as email client.

;;; Code:

(use-package mu4e
  :config
  (setq mu4e-mu-binary (concat emma-user-local-dir "bin/mu")
        mu4e-maildir "~/.Maildir"
        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp"
        message-sendmail-envelope-from 'header
        mu4e-get-mail-command "offlineimap"
        mu4e-compose-context-policy 'ask-if-none
        mu4e-context-policy 'pick-first
        mu4e-view-show-images t
        mu4e-view-image-max-width 800
        mu4e-index-update-in-background nil
        user-full-name "Gabriel Lievano")
  (setq mu4e-contexts
        `(,(make-mu4e-context
            :name "Fastmail"
            :match-func
            (lambda (msg) (when msg
                            (string-prefix-p
                             "/jglievano-fastmail.com"
                             (mu4e-message-field msg :maildir))))
            :vars
            '((user-mail-address . "gabe@jglievano.com")
              (mu4e-sent-folder . "/jglievano-fastmail.com/Sent")
              (mu4e-drafts-folder . "/jglievano-fastmail.com/Drafts")
              (mu4e-trash-folder . "/jglievano-fastmail.com/Trash")
              (mu4e-refile-folder . "/jglievano-fastmail.com/Archive")
              (mail-reply-to "gabe@jglievano.com")
              (setq message-sendmail-extra-arguments (list -a "Fastmail")))))))

(provide 'mod-email)
;;; mod-email.el ends here
