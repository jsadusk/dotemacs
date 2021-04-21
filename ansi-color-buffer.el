(defgroup ansi-color-buffer nil
  "Apply ansi colors to buffer"
  :group 'tools)

(load-library "ansi-color")

(defun ansi-color-buffer ()
  "Apply ansi color to buffer"
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'ansi-color-buffer)
