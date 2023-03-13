(defun to-shreds-you-say ()
  "To shreds you say."
  (interactive)
  (switch-to-buffer (get-buffer-create "*shreds*"))
  (erase-buffer)
  (sit-for 0)
  (animate-string "To shreds you say!"
		  (/ (window-height) 2) (- (/ (window-width) 2) 12)))
