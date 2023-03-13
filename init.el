;; packages
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;; Start emacs server
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;; change modifier keys
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq mac-control-modifier 'control)

;; flycheck
;; no more requires
(setq-default flycheck-emacs-lisp-load-path 'inherit)
;; disable unnecessary flycheck stuff in elisp
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

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
		      git-gutter git-gutter-fringe
		      avy
		      web-mode
		      rjsx-mode
		      ivy-rich
		      which-key
		      yaml-mode
		      lsp-java
		      hl-todo
		      web-mode
		      counsel-projectile
		      darkroom
		      ace-window
		      typescript-mode))

;; Install all used packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-hook 'after-init-hook 'toggle-frame-maximized)

;; global bindings
(global-set-key (kbd "C-x j") 'eshell)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

(which-key-mode)

(add-hook 'darkroom-mode-hook #'(lambda ()
				  ;; (my-buffer-face-mode-fixed)
				  (turn-on-visual-line-mode)
				  (linum-mode -1)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; visual bell
(setq visible-bell 1)
;; flash mode line instead of awful icon
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

;; ace
(require 'ace-window)
(setq aw-keys '(?j ?k ?l ?\; ?a ?s ?d ?f))

(defun econf ()
  "Open Emacs config."
  (interactive)
  (let ((e-files (cl-remove-if-not (lambda (f)
				     (string-match-p "^[a-zA-Z0-9]*\.el$" (format "%s" f))) 
                                   (directory-files "~/.emacs.d/conf"))))
    (find-file (concat "~/.emacs.d/conf/" (completing-read "Emacs config files: " e-files)))))

;; spellcheck
(require 'ispell)
(setq ispell-program-name "hunspell")
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
(require 'flyspell)
(setq flyspell-issue-message-flag nil)
(global-set-key (kbd "<f8>") 'flyspell-buffer)

;; Loading other configs
(load "~/.emacs.d/conf/editting.el")
(load "~/.emacs.d/conf/prog.el")
(load "~/.emacs.d/conf/ivy.el")
(load "~/.emacs.d/conf/projectile.el")
(load "~/.emacs.d/conf/sw.el")
(load "~/.emacs.d/conf/git.el")
(load "~/.emacs.d/conf/clojure.el")
(load "~/.emacs.d/conf/text.el")
(load "~/.emacs.d/conf/shell.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(sanityinc-tomorrow-night))
 '(custom-safe-themes
   '("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))
 '(package-selected-packages
   '(color-theme-sanityinc-tomorrow zenburn-theme neotree typescript-mode yasnippet adoc-mode jdecomp darkroom yaml-mode counsel-projectile which-key ivy-rich rjsx-mode hl-todo ace-window avy git-gutter-fringe web-mode jarchive exec-path-from-shell s eglot hugsql-ghosts git-gutter company ripgrep flycheck use-package projectile multiple-cursors cider monokai-theme rainbow-delimiters clojure-mode ivy magit paredit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
