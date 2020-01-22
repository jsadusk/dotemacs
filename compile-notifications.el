(defgroup compile-notifications nil
  "Send desktop notifications when compilation fininshes and emacs doesn't have focus"
  :group 'tools)

(add-hook 'focus-in-hook '(lambda ()
			    (setq frame-has-focus 1)))
(add-hook 'focus-out-hook '(lambda ()
			    (setq frame-has-focus nil)))

(defun my-compilation-finish-function (buffer desc)
  (if (string-match "^finished" desc)
      (setq notify-msg "Compilation finished")
    (setq notify-msg (concat "Compilation failed: " (string-trim desc)))
    )
  (if frame-has-focus
      (message notify-msg)
    (notifications-notify :title notify-msg)))

(defun compile-notifications ()
  "Send desktop notifications when compilation fininshes and emacs doesn't have focus"
  (interactive)
  (add-hook 'compilation-finish-functions 'my-compilation-finish-function)
  )

(provide 'compile-notifications)
