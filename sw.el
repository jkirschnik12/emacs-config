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


(defun get-jira (ticket)
  "Open jira TICKET by number."
  (interactive "sPlease enter a ticket number: ")
  (browse-url (concat "https://jira.singlewire.lan/browse/" ticket)))

(defun jira-thing-at-point ()
  "Blah."
  (interactive)
  (let ((pos (thing-at-point 'sexp)))
    (get-jira pos)))

(global-set-key (kbd "C-c j") 'jira-thing-at-point)

