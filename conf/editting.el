;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(set-face-attribute 'default nil :height 120)
(setq create-lockfiles nil)
(setq inhibit-startup-message t)

(require 'cl-lib)

;; (defun one-window-hook ()
;;   "Make font size bigger when there's only one buffer open."
;;   (let ((wc (count-windows)))
;;     (if (or (= wc 1) (and (= wc 2) (or (active-minibuffer-window)
;;                     (cl-remove-if-not (lambda (w)
;;                             (string-match-p (regexp-quote "*which-key*") (format "%s" w))) (window-list-1)))))
;;  (set-face-attribute 'default nil :height 140)
;;       (set-face-attribute 'default nil :height 120))))

;; (add-hook 'window-configuration-change-hook
;;           'one-window-hook)

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
(global-display-line-numbers-mode)

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
(global-set-key (kbd "<mouse-2>") 'mc/add-cursor-on-click)

;; Comments
(global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-;") 'comment-line)

;; delete-indentation
(global-set-key (kbd "C-c C-d") 'delete-indentation)
;;(global-set-key (kbd "M"))

(require 'swiper)
(global-set-key (kbd "C-M-s") 'swiper-thing-at-point)


;; More standard-like C-backspace
(defun my-kill-back ()
  (interactive)
  (if (bolp)
      (backward-delete-char 1)
    (if (string-match "[^[:space:]]" (buffer-substring (point-at-bol) (point)))
        (backward-kill-word 1)
      (delete-region (point-at-bol) (point)))))

(global-set-key [C-backspace] 'my-kill-back)

(defun my-paredit-delete ()
  "Intellij like delete where indentation is deleted when all chars from bol to cursor are blank."
  (interactive)
  (if (string-match "^\s*$" (buffer-substring (point-at-bol) (point)))
      (delete-indentation)
    (paredit-backward-delete)))

(require 'paredit)
(define-key paredit-mode-map (kbd "DEL") 'my-paredit-delete)

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
        ("ask" . "#09e237")
        ("DEBUG"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("STUB"   . "#1E90FF")
        ("stub"   . "#1E90FF")))

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(setq-default fill-column 120)

(setq tab-width 4)
