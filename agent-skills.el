;;; agent-skills.el --- Functions to be accessed by agents via MCP -*- lexical-binding: t -*-

;; Safe approved functions meant to be accessed by agents via emacs-mcp-server

(require 'project)

(defun safe-find-project-file (filename)
  "Visit FILENAME after confirmation, only if under the project."
  (unless (and filename (> (length filename) 0))
    (user-error "No file"))
  (let* ((proj (or (project-current)
                   (user-error "No current project")))
         (root (project-root proj)))
    (unless (file-in-directory-p filename root)
      (user-error "Not under project root %s:\n%s" root filename))
    (if (y-or-n-p (format "Open [%s]?" filename))
        (find-file-other-window filename)
      (message "Canceled"))))

(defun get-compilation-contents ()
  (with-current-buffer "*compilation*"
    (buffer-substring-no-properties (point-min) (point-max))))

(defun safe-compile (command)
  (if (y-or-n-p (format "Compile [%s]?" command))
    (let ((compile-command command)
          (compilation-finish-functions (cons (lambda (b d) (delete-file "/tmp/.emacs-compile")) compilation-finish-functions)))
      (write-region "" nil "/tmp/.emacs-compile")
      (call-interactively 'project-compile))))

(provide 'agent-skills)

;;; safe-find-project-file.el ends here
