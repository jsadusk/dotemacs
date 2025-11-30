;; -*- lexical-binding: t; -*-

(defgroup reopen-buffers nil
  "Utilities for reopening remote buffers"
  :group 'tools)

(defun reopen-buffer (&optional thisbuffer)
  (interactive)
  (let* (
         (buff (if thisbuffer thisbuffer (current-buffer)))
         (oldpoint (with-current-buffer buff (point)))
         (oldmark (with-current-buffer buff (mark)))
         (win (get-buffer-window buff))
         (file (buffer-file-name buff))
         )
    (if file
        (progn
          (kill-buffer buff)
          (let ((newbuff (find-file-noselect file)))
            (with-current-buffer newbuff
              (goto-char oldpoint)
              (set-mark oldmark))
            (if win
                (with-selected-window win (switch-to-buffer newbuff))
              )
            )
          )
      )
    )
  )

(defun reopen-buffers-on-remote (&optional remote)
  (interactive)
  (let ((remote (if remote remote (file-remote-p (buffer-file-name)))))
    (if remote
        (progn
          (dolist (buff (buffer-list))
            (message "Trying reopen on %S" buff)
            (let* (
                   (thisfile (buffer-file-name buff))
                   (thisremote (if thisfile (file-remote-p thisfile) nil))
                   )
              (if (and thisremote (string= thisremote remote))
                  (reopen-buffer buff)
                )
              )
            )
          )
      )
    )
  )

(provide `reopen-buffers)
