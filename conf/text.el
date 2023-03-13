;; org mode babel magic
(require 'org)
(require 'ob-clojure)
(require 'cider)
(setq org-babel-clojure-backend 'cider)
(setq org-confirm-babel-evaluate nil)


;; Helpful functions for notes
(defun today ()
  "Open an entry for today org mode."
  (interactive)
  (let ((today (format-time-string "%Y-%m-%d"))
	(dir "~/notes/daily/"))
    (find-file (concat dir today ".org"))))

(defun yesterday ()
  "Open an entry for yesterday if exists org mode."
  (interactive)
  (let ((yesterday (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 1))))
	(dir "~/notes/daily/"))
    (find-file (concat dir yesterday ".org"))))

(require 'counsel)
(defun search-notes ()
  "Search daily notes."
  (interactive)
  (let ((dir "~/notes/daily/"))
    (counsel-rg "" dir)))

(defun buffer-org ()
  "Org buffer switcher."
  (interactive)
  (let ((org-buffers (mapcar #'buffer-name (cl-remove-if-not (lambda (b) (string-match-p "\.org$" (format "%s" b))) (buffer-list)))))
    (if (length> org-buffers 0)
	(if (length= org-buffers 1)
	    (switch-to-buffer (car org-buffers))
	  (switch-to-buffer (completing-read "Choose an org mode buffer: " org-buffers))))))

(defun todo ()
  "Jump to todo list."
  (interactive)
  (find-file "~/notes/todo.org"))

;; Changing views
(add-hook 'org-mode-hook (lambda ()
			   (visual-line-mode)
			   (darkroom-mode)
			   (display-line-numbers-mode 0)))
