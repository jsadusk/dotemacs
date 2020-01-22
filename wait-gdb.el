(defgroup wait-gdb nil
  "Start gdb from emacsclient with a process sentinal so an external process can wait until its done"
  :group 'tools)

(setq gud-running 0)

(defun wait-gdb (cmd subcmd)
  (realgud:gdb cmd)
  (setq gud-running 1)

  (set-process-sentinel
   (get-buffer-process (concat "*gdb shell*"))
   (lambda (process event)
     (setq gud-running 0)
     (kill-buffer (process-buffer process))))
  nil
  )

(provide `wait-gdb)
