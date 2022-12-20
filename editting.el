;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(set-face-attribute 'default nil :height 120)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)

;; UI improvements
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(global-linum-mode)

;; Clipboard improvements
(setq
 select-enable-clipboard t
 select-enable-primary t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t)

 ;; Highlight matching paren
 (show-paren-mode 1)


;; Multiple cursor magic
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Comments
(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-;") 'comment-line)

;; delete-indentation
(global-set-key (kbd "C-c C-d") 'delete-indentation)
;;(global-set-key (kbd "M"))

(require 'swiper)
(global-set-key (kbd "C-M-s") 'swiper-thing-at-point)

;; delete
(global-set-key [C-M-backspace] 'delete-indentation)


;; More standard-like C-backspace
(defun my-kill-back ()
  (interactive)
  (if (bolp)
      (backward-delete-char 1)
    (if (string-match "[^[:space:]]" (buffer-substring (point-at-bol) (point)))
        (backward-kill-word 1)
      (delete-region (point-at-bol) (point)))))

(global-set-key [C-backspace] 'my-kill-back)
;; Prefer vertical split
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 80)

;; spelling
;; (dolist (hook '(text-mode-hook clojure-mode-hook))
;;       (add-hook hook (lambda () (flyspell-mode 1))))
;;     (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
;;       (add-hook hook (lambda () (flyspell-mode -1))))

;; (setq flyspell-issue-message-flag nil)

(defun toggle-frame-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

;; I don't use the default binding of 'C-x 5', so use toggle-frame-split instead
(global-set-key (kbd "C-x 5") 'toggle-frame-split)


;; avy
(require 'avy)
;; (global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "C-'") 'avy-goto-word-1)
(setq avy-all-windows nil)

(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;; hl-todo
(require 'hl-todo)
(global-hl-todo-mode)
(define-key hl-todo-mode-map (kbd "C-c ; p") 'hl-todo-previous)
(define-key hl-todo-mode-map (kbd "C-c ; n") 'hl-todo-next)

(setq hl-todo-keyword-faces
	'(("TODO"   . "#ffe700")
	  ("todo"   . "#ffe700")
	  ("FIXME"  . "#FF0000")
	  ("DEBUG"  . "#A020F0")
	  ("GOTCHA" . "#FF4500")
	  ("STUB"   . "#1E90FF")))

