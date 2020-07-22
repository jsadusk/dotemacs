(defgroup home-layouts.el nil
  "Layout functions for my home setup"
  :group 'tools)

(defun docked-dual-layout ()
  "Layout for home with two monitors"
  (interactive)
                                        ;(message (pp-to-string (display-monitor-attributes-list)))
  (let ((lastframe (selected-frame)))
    (seq-map
     (lambda (otherframe)
       (delete-frame otherframe))
     (cdr (frame-list)))

    (seq-map
     (lambda (disp)
       (unless (string= "eDP-1-1" (cdr (assoc 'name disp)))
         (let ((workarea (assoc 'workarea disp)) (frame (make-frame)))
           (select-frame frame)
           (set-frame-position frame (nth 1 workarea) (nth 2 workarea))
           (set-frame-font "Inconsolata-10")
           (toggle-frame-maximized)
           (toggle-frame-fullscreen)
           )))
     (display-monitor-attributes-list))

    (delete-frame lastframe)
    (seq-map
     (lambda (frame)
       (select-frame frame)
       ;(smart-split)
       )))
    
                                        ;(set-frame-position (selected-frame) 0 0)
  ;(toggle-frame-fullscreen)
  )
