(require 'package)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)
(server-start)

(defvar my-packages
  '(paredit
    smex
    company
    magit
    ivy
    ;; git-gutter
    git-gutter-fringe
    git-gutter
					;    multiple-cursors
					;    json-reformat
    clojure-mode
    clojure-mode-extra-font-locking
    ido-completing-read+
    rainbow-delimiters
    monokai-theme
    flycheck
    flycheck-clj-kondo
    lsp-mode
    lsp-ui
    magit
    use-package
    cider
    multiple-cursors ripgrep
					;    origami ;; code-folding
					;    treemacs
					;    lsp-treemacs
					;    find-file-in-project
    projectile				;; projectile-grep
					;    highlight-symbol
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'rainbow-delimiters)
(global-git-gutter-mode +1)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

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

(set-face-attribute 'default nil :height 120)

;; Smex
;(smex-initialize)
;(global-set-key (kbd "M-x") 'smex)
;; ido (auto complete)
; (ido-mode t)
; (ido-ubiquitous-mode t)
; (setq ido-everywhere t)
  ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
; (setq ido-enable-flex-matching t)
; (setq ido-use-filename-at-point nil)
; (setq ido-file-extensions-order '(".clj" ".sql" ".ts" ".tsx" ".edn" ".txt"))
(global-set-key (kbd "C-x C-b") 'ibuffer)



;; 
;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))


;; commenting stuff
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; Projectile
(require 'ivy)
(projectile-mode)
(setq projectile-completion-system 'ivy)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq projectile-sort-order 'recentf)
(setq ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy)
        (t . ivy--regex-plus)))

;; Editting

;; Highlight matching parens
(show-paren-mode 1)

;; Highlight current line
;;(global-hl-line-mode 0)

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
;; cider company mode hints
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
;; eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'eldoc-mode)

(global-set-key (kbd "TAB") #'company-indent-or-complete-common)

;; Misc
(setq create-lockfiles nil)
(setq inhibit-startup-message t)



;; flycheck
(require 'flycheck-clj-kondo)
(global-flycheck-mode)

;; unique buffer names
(require 'uniquify)
(setf uniquify-buffer-name-style 'forward)


;; git gutter
(require 'git-gutter-fringe)
(global-git-gutter-mode)

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
	     (setq lsp-clojure-server-command '("/usr/local/bin/clojure-lsp")))

(setq lsp-enable-on-type-formatting nil)
(setf lsp-ui-sideline-show-code-actions nil)
(setq lsp-lens-enable nil)
(setq lsp-modeline-diagnostics-enable nil)

;; lsp with previews
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(global-company-mode)

(use-package company
  :ensure t)

(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-ui-doc-show-with-cursor nil)

;; disable lsp formatting, _really_ slow
(setq lsp-enable-indentation nil)


;;cider stuff
(setq cider-lein-command "/usr/local/bin/lein")
(setq cider-save-file-on-load t)


;; ivy autocomplete
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(counsel-mode)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper)


;; toggle vertiacal horizontal buffers
(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

;; I don't use the default binding of 'C-x 5', so use toggle-frame-split instead
(global-set-key (kbd "C-x 5") 'toggle-frame-split)



;; Nick's git command
(require 'magit)
(defun magit-commit-msg-prefix ()
  "Search COMMIT_EDITMSG buffer for branch name, and paste at the
beginning of the buffer."
  (interactive)
  (search-forward-regexp "On branch .*SW-")
  (backward-char 3)
  (let ((beg (point)))
    (kill-word 2)
    (yank)
    (beginning-of-buffer)
    (yank)
    (insert " ")))

;; Bind to Control-c, Control-u
(define-key git-commit-mode-map (kbd "C-c C-u") 'magit-commit-msg-prefix)


;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;eshell
(global-set-key (kbd "C-s-t") 'eshell)

;; Added by emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:update-interval 2)
 '(lsp-ui-doc-enable nil)
 '(package-selected-packages
   '(multiple-cursors ripgrep web-mode flx counsel ivy use-package git-gutter-fringe exec-path-from-shell flycheck-clj-kondo flycheck tagedit smex rainbow-delimiters projectile paredit monokai-theme magit ido-completing-read+ clojure-mode-extra-font-locking cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
