(defgroup compile-notifications nil
  "Send desktop notifications when compilation fininshes and emacs doesn't have focus"
  :group 'tools)

(defun xp-notify (title message)
  (if (eq system-type 'darwin)
      (do-applescript (concat "display notification \"" message "\" with title \"" title "\""))
    (notifications-notify :title title :body message))
  )

(defun my-compilation-finish-function (buffer desc)
  (let ((compile-frame (window-frame (get-buffer-window buffer))))
                                        ;(if (not (frame-focus-state compile-frame))
    
        (if (string-match "^finished" desc)
            (xp-notify "Compilation finished" "")
          (xp-notify "Compilation failed" (string-trim desc))
          )
      ;)
    )
  )

(defun compile-notifications ()
  "Send desktop notifications when compilation fininshes and emacs doesn't have focus"
  (interactive)
  (add-hook 'compilation-finish-functions 'my-compilation-finish-function)
  )

(provide 'compile-notifications)
