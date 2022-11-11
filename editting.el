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

