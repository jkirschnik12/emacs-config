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

;; diff-hl
;(global-diff-hl-mode)

;; commenting
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-;") 'toggle-comment-on-line)
