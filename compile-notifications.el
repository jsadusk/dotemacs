;; -*- lexical-binding: t; -*-
(defgroup compile-notifications nil
  "Send desktop notifications when compilation fininshes and emacs doesn't have focus"
  :group 'tools)

(require 'xp-notifications)

(defun my-compilation-finish-function (buffer desc)
  (let ((compile-frame (window-frame (get-buffer-window buffer))))
    (if (not (frame-focus-state compile-frame))
        (if (string-match "^finished" desc)
            (xp-notify "Compilation finished" "")
          (xp-notify "Compilation failed" (string-trim desc))
          )
      )
    )
  )

(defun compile-notifications ()
  "Send desktop notifications when compilation fininshes and emacs doesn't have focus"
  (interactive)
  (add-hook 'compilation-finish-functions 'my-compilation-finish-function)
  )

(defun delay-focus-msg ()
  (interactive)
  (let ((testframe (selected-frame)))
    (sleep-for 5)
    (message (prin1-to-string (frame-focus-state testframe)))
    )
  
  )

(provide 'compile-notifications)
