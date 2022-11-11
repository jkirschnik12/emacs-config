(defun magit-commit-msg-prefix ()
  "Search COMMIT_EDITMSG buffer for branch name, and paste at the
beginning of the buffer."
  (interactive)
  (search-forward-regexp "On branch .*SW-")
  (backward-char 3)
  (let ((beg (point)))
    (kill-word 2)
    (yank)
    (beginning-of-buffer)
    (yank)
    (insert " ")))

(require 'magit)
;; Bind to Control-c, Control-u
(define-key git-commit-mode-map (kbd "C-c C-u") 'magit-commit-msg-prefix)
