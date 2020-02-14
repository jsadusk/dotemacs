(defgroup recieve-notifications nil
  "Receive desktop notifications"
  :group 'tools)

(setq notification-monitor-process nil)

(setq display-notification-messages 1)

(add-hook 'focus-in-hook '(lambda ()
			    (setq display-notification-messages 1)))
(add-hook 'focus-out-hook '(lambda ()
			    (setq display-notifications-messages nil)))


(defun sentinel (p e)
  (message (concat "event: " e))
  (setq notification-monitor-process nil))

(defun filter (p s)
  (if display-notification-messages
      (let ((pieces (split-string s "|")))
        (unless (string= (nth 0 pieces) "Emacs")
          (message "** %s: %s\n%s" (nth 0 pieces) (nth 1 pieces) (nth 2 pieces))))))

(defun receive-notifications ()
  "Notification messages"
  (interactive)
  (when notification-monitor-process
    (delete-process notification-monitor-process))
  (let ((command "python3 ~/.emacs.lib/notifygetter.py"))
    (let ((process (start-process-shell-command command (current-buffer) command)))
      (set-process-sentinel process 'sentinel)
      (set-process-filter process 'filter)
      (setq notification-monitor-process process))))

(provide 'receive-notifications)
