(defun init ()
  "Jump to init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun econf ()
  "Open Emacs config."
  (interactive)
  (let ((e-files (cl-remove-if-not (lambda (f)
				                     (string-match-p "^[a-zA-Z0-9]*\.el$" (format "%s" f))) 
                                   (directory-files "~/.emacs.d/conf"))))
    (find-file (concat "~/.emacs.d/conf/" (completing-read "Emacs config files: " e-files)))))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses 
   `flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
      (progn 
        (message "Flyspell off")
        (flyspell-mode -1))
    (progn
      (message "Flyspell on")
      (flyspell-mode))))

(defun sql-scratch ()
  (interactive)
  (find-file "~/code/scratch.sql"))
