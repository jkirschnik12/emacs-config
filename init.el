(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

;;Open new files in current emacs instance
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'control)


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
    lsp-mode
    s
    lsp-ivy
    company
    eglot
    exec-path-from-shell
    jarchive
    lsp-ui
    lsp-ivy
    git-gutter git-gutter-fringe
    avy
    web-mode
    tide
    rjsx-mode
    ivy-rich
    which-key
))

;; just for eshell convenience
(defalias 'ff 'find-file)
(which-key-mode)

;; Install all used packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'ivy-rich)
(ivy-rich-mode 1)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)

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
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-selection-wrap-around t)

;; visual bell
(setq visible-bell 1)
;; flash modeline instead of awful icon
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))

;; open init file
(defun init ()
  "Jump to init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(global-set-key (kbd "C-c i") 'counsel-imenu)


;; Loading other configs
(load "~/.emacs.d/editting.el")
(load "~/.emacs.d/lisp.el")
(load "~/.emacs.d/ivy.el")
(load "~/.emacs.d/projectile.el")
(load "~/.emacs.d/sw.el")
(load "~/.emacs.d/git.el")
(load "~/.emacs.d/tide.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(counsel-projectile which-key ivy-rich rjsx-mode hl-todo ace-window avy git-gutter-fringe web-mode jarchive exec-path-from-shell tide s eglot hugsql-ghosts git-gutter company ripgrep flycheck use-package projectile multiple-cursors cider monokai-theme rainbow-delimiters clojure-mode ivy magit paredit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
