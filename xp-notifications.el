;;; xp-notifications --- Cross platform notifications

;;; Commentary:

;; A wrapper around various notification systems to allow one function to notify on all platforms and configurations

;;; Code:

(require 'notifications)

(defgroup xp-notifications nil
  "Cross platform wrapper for various notification mechanisms."
  :group 'tools)

(defvar xp-notifications/style-default)
(setq xp-notifications/style-default
      (cond
       ((eq system-type 'darwin) "growl")
       ((eq system-type 'gnu/linux) "dbus")
       )
      )

(defcustom xp-notifications/style xp-notifications/style-default
  "Style for notifications."
  :type '(string)
  :group 'xp-notifications)

(defvar xp-notifications/reverse-ssh-host-default)
(setq xp-notifications/reverse-ssh-host-default
      (if (getenv "SSH_CLIENT")
          (car (split-string (getenv "SSH_CLIENT") " "))
        ""))

(defcustom xp-notifications/reverse-ssh-host
  xp-notifications/reverse-ssh-host-default
  "Default host to do reverse ssh notifications."
  :type '(string)
  :group 'xp-notifications
  )

(defcustom xp-notifications/reverse-ssh-port
  22
  "Default port to do reverse ssh notifications."
  :type '(integer)
  :group 'xp-notifications
  )

(defcustom xp-notifications/do-reverse-ssh
  nil
  "Do notifications via reverse ssh."
  :type '(boolean)
  :group 'xp-notifications
  )

(defun xp-notifications/reverse-ssh-command (command)
  "COMMAND -- Run COMMAND via reverse ssh."
  (message "reverse ssh")
  (let ((portarg (if xp-notifications/reverse-ssh-port
                     (format "-p %d " xp-notifications/reverse-ssh-port)
                   ""
                   )))
    (message (format "ssh %s %s %s"
                     portarg
                     xp-notifications/reverse-ssh-host
                     command))
    (shell-command (format "ssh %s %s %s"
                           portarg
                           xp-notifications/reverse-ssh-host
                           command))
    )
  )

(defun xp-notifications/growl-script (appname title message)
  "APPNAME TITLE MESSAGE -- command for growl notification."
  (format "\"tell application \\\"System Events\\\"
set frontApp to name of first application process whose frontmost is true
end tell
if frontApp is not \\\"%s\\\" then
    display notification \\\"%s\\\" with title \\\"%s\\\"
end if\"" appname message title)
  )

(defun xp-notify (title message)
  "TITLE MESSAGE --- Send a cross platform notification."
  (interactive)
  (message "xp-notify style")
  (message xp-notifications/style)
  (cond
   ((string= xp-notifications/style "dbus")
    (message "dbus")
    (if xp-notifications/do-reverse-ssh
        (xp-notifications/reverse-ssh-command
         (format "notify-send \"%s\" \"%s\"" title message)
         )
      (notifications-notify :title title :body message)
      )
    )
   ((string= xp-notifications/style "growl")
    (message "growl")
    (if xp-notifications/do-reverse-ssh
        (xp-notifications/reverse-ssh-command
         (format "osascript -e '%s'" (xp-notifications/growl-script "X11.bin" title message)))
      (do-applescript (xp-notifications/growl-script "Emacs" title message))
    )
    )
   ('t
    (message (concat "unknown: " xp-notifications/style))
    )
   )
  )


(provide 'xp-notifications)
;;; xp-notifications.el ends here
