;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(set-face-attribute 'default nil :height 120)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)

(defun font-12 ()
  "Change font to height 12."
  (interactive)
  (set-face-attribute 'default nil :height 120))

(defun font-14 ()
  "Change font to height 14."
  (interactive)
  (set-face-attribute 'default nil :height 140))

(defun font-16 ()
  "Change font to height 14."
  (interactive)
  (set-face-attribute 'default nil :height 160))

;; UI improvements
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(add-to-list 'default-frame-alist '(undecorated . t))
(global-display-line-numbers-mode)

;; Clipboard improvements
(setq
 select-enable-clipboard t
 select-enable-primary t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t)

;; Highlight matching paren
(show-paren-mode 1)

;; Comments
(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-;") 'comment-line)

;; delete-indentation
(global-set-key (kbd "C-c C-d") 'delete-indentation)

;; More standard-like C-backspace
(defun my-kill-back ()
  (interactive)
  (if (bolp)
      (delete-char 1)
    (if (string-match "[^[:space:]]" (buffer-substring (pos-bol) (point)))
        (backward-kill-word 1)
      (delete-region (pos-bol) (point)))))

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

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)
(setq-default fill-column 120)

(setq tab-width 4)

(setq visible-bell 1)
;; flash mode line instead of awful icon
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))
