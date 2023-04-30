;; fringe doesn't work in term version
(when (window-system)
  (require 'git-gutter-fringe))

(require 'git-gutter)
(require 'git-gutter-fringe)

(global-git-gutter-mode +1)

;; stolen from nick
(defun my-next-git-gutter-diff (arg)
  (interactive "p")
  (git-gutter:next-diff arg)
  (recenter))

(defun my-prev-git-gutter-diff (arg)
  (interactive "p")
  (git-gutter:previous-diff arg)
  (recenter))

(global-set-key (kbd "C-M-3") 'my-next-git-gutter-diff)
(global-set-key (kbd "C-M-2") 'my-prev-git-gutter-diff)
(global-set-key (kbd "C-c g d") 'git-gutter:popup-hunk)
(global-set-key (kbd "C-c g r") 'git-gutter:revert-hunk)
(global-set-key (kbd "C-c g SPC") 'git-gutter:mark-hunk)


(define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
(define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)

(global-set-key (kbd "s-i") 'vc-msg-show)
