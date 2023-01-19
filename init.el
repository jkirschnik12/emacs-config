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
(setq mac-control-modifier 'super)

(require 'org)

(add-hook 'org-mode-hook (lambda ()
			  (visual-line-mode)
			  (darkroom-mode)
			  (linum-mode -1)))

(add-hook 'after-init-hook 'toggle-frame-maximized)
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
		      rjsx-mode
		      ivy-rich
		      which-key
		      yaml-mode
		      lsp-java
		      ))


(global-set-key (kbd "C-x j") 'eshell)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

;; just for eshell convenience
(defalias 'ff 'find-file)

(require 'eshell)
(defun de ()
  "change dir to dev-env in eshell"
  (interactive)
  (eshell/cd "~/code/dev-environment"))

(defalias 'cl 'eshell/clear-scrollback)

(defun disable-line-num ()
  (linum-mode 0))

(add-hook 'eshell-mode-hook #'disable-line-num)
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

(defun todo ()
  "jump to todo list"
  (interactive)
  (find-file "~/notes/todo.org"))

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(global-set-key (kbd "C-c i") 'counsel-imenu)

;; no company completion in eshell
(setq company-global-modes '(not eshell-mode))

;; yaml files
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; switch to org

(defun buffer-org ()
  "Org buffer switcher."
  (interactive)
  (let ((org-buffers (mapcar #'buffer-name (cl-remove-if-not (lambda (b) (string-match-p "\.org$" (format "%s" b))) (buffer-list)))))
    (if (length> org-buffers 0)
	(if (length= org-buffers 1)
	    (switch-to-buffer (car org-buffers))
	  (switch-to-buffer (completing-read "Choose an org mode buffer: " org-buffers))))))

(defun econf ()
  "Open Emacs config."
  (interactive)
  (let ((e-files (cl-remove-if-not (lambda (f)
				     (string-match-p "^[a-zA-Z0-9]*\.el$" (format "%s" f))) (directory-files "~/.emacs.d"))))
    (find-file (concat "~/.emacs.d/" (completing-read "Emacs config files: " e-files)))))

(defun today ()
  "Open an entry for today org mode."
  (interactive)
  (let ((today (format-time-string "%Y-%m-%d"))
	(dir "~/notes/daily/"))
    (find-file (concat dir today ".org"))))

(defun yesterday ()
  "Open an entry for yesterday if exists org mode."
  (interactive)
  (let ((yesterday (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 1))))
	(dir "~/notes/daily/"))
    (find-file (concat dir yesterday ".org"))))

(defun search-notes ()
  "Search daily notes."
  (interactive)
  (let ((dir "~/notes/daily/"))
    (counsel-rg "" dir)))

(defun tmp ()
  "T."
  (interactive)
  (counsel-projectile-rg "--type clojure"))

;; Org mode magic
(require 'org)
(require 'ob-clojure)
(require 'cider)
(setq org-babel-clojure-backend 'cider)
(setq org-confirm-babel-evaluate nil)

;;uh

(customize-set-variable 'jdecomp-decompiler-paths
                        '((cfr . "~/cfr-0.152.jar")))

;; Loading other configs
(load "~/.emacs.d/editting.el")
(load "~/.emacs.d/lisp.el")
(load "~/.emacs.d/ivy.el")
(load "~/.emacs.d/projectile.el")
(load "~/.emacs.d/sw.el")
(load "~/.emacs.d/git.el")
(load "~/.emacs.d/clojure.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(jdecomp darkroom yaml-mode counsel-projectile which-key ivy-rich rjsx-mode hl-todo ace-window avy git-gutter-fringe web-mode jarchive exec-path-from-shell s eglot hugsql-ghosts git-gutter company ripgrep flycheck use-package projectile multiple-cursors cider monokai-theme rainbow-delimiters clojure-mode ivy magit paredit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
