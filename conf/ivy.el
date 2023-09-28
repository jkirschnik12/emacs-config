;;; Package -- Summary
;;; Commentary:

(require 'ivy)

(ivy-mode 1)

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-height 10)
(setq ivy-wrap t)

;; counsel
(counsel-mode)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

(defun ivy-switch-buffer-all ()
  (interactive)
  (let ((ivy-ignore-buffers '()))
    (ivy-switch-buffer)))
;; ivy
(setq ivy-ignore-buffers '("^\\*"))
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x B") 'ivy-switch-buffer-all)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; swiper
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)

(require 'ivy-rich)
(ivy-rich-mode 1)
;; ???
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)


