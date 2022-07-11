(require 'package)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

;; UI improvements
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(scroll-bar-mode -1)

;; Line numbers
(global-linum-mode)

(setq make-backup-files nil)

(setq
 select-enable-clipboard t
 select-enable-primary t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t)

(load-theme 'monokai t)

(set-face-attribute 'default nil :font "Source Code Pro semibold" :height 120)

;; Smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; ido (auto complete)
(ido-mode t)
(ido-ubiquitous-mode t)

;; Projectile
(projectile-mode)

;; Editting

;; Highlight matching parens
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Lisp paredit and doc stuff
(autoload 'enable-paredit-mode "paredit" "structural editting for lisps" t)
(add-hook 'emacs-list-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook       #'enable-paredit-mode)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)

;; Misc
(setq create-lockfiles nil)
(setq inhibit-startup-message t)



;; flycheck
(require 'flycheck-clj-kondo)
(global-flycheck-mode)



;; clojure-lsp
(require 'lsp-mode)
(require 'use-package)
(use-package lsp-mode
	     :ensure t
	     :hook ((clojure-mode . lsp)
		    (clojurec-mode . lsp)
		    (clojurescript-mode . lsp))
	     :config
	     ;; add paths to your local installation of project mgmt tools, like lein
	     (setenv "PATH" (concat
			     "/usr/local/bin" path-separator
			     (getenv "PATH")))
	     (dolist (m '(clojure-mode
			  clojurec-mode
			  clojurescript-mode
			  clojurex-mode))
	       (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
	     ;(setq lsp-clojure-server-command '("/path/to/clojure-lsp"))
	     );; Optional: In case `clojure-lsp` is not in your $PATH

;; lsp with previews
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company
  :ensure t)





;; Added by emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-mode flycheck-clj-kondo flycheck tagedit smex rainbow-delimiters projectile paredit monokai-theme magit ido-completing-read+ clojure-mode-extra-font-locking cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
