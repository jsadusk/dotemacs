(defgroup smart-split nil
  "Split a frame into 80 char columns"
  :group 'tools)


(defun smart-split ()
  "Split the frame into 80-column sub-windows, and make sure no window has
   fewer than 80 columns."
  (interactive)
  (defun smart-split-helper (w)
    "Helper function to split a given window into two, the first of which has 
     80 columns."
    (if (> (window-width w) (* 2 81))
    (let ((w2 (split-window w 83 t)))
      (smart-split-helper w2))))
  (smart-split-helper nil))

(provide `smart-split)
