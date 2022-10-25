(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)
(server-start)

(defvar my-packages '(
    paredit
    magit
    ivy
    counsel
    clojure-mode
    rainbow-delimiters
    monokai-theme
    cider
    multiple-cursors
    projectile
    use-package
    flycheck
    flycheck-clj-kondo
    ripgrep
    swiper
    eglot
    company
    diff-hl))

;; Install all used packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Rainbow parens
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Theme
(load-theme 'monokai t)

;; flycheck
(require 'flycheck-clj-kondo)
(global-flycheck-mode)

;; eldoc-mode
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)

;; company
(add-hook 'after-init-hook 'global-company-mode)
(setq company-selection-wrap-around t)

;; find-refs
(define-key clojure-mode-map (kbd "C-c C-r C-r") 'xref-find-references)


;; Loading other configs
(load "~/.emacs.d/editting.el")
(load "~/.emacs.d/lisp.el")
(load "~/.emacs.d/ivy.el")
(load "~/.emacs.d/projectile.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(eglot elgot diff-hl company ripgrep flycheck use-package projectile multiple-cursors cider monokai-theme rainbow-delimiters clojure-mode ivy magit paredit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
