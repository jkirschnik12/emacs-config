(require 'clojure-mode)
(require 'eglot)
(require 'cider)
(require 'eldoc)
(require 'use-package)
(require 'lsp-ui)
(require 'lsp)
(require 'lsp-headerline)
(require 'web-mode)

(autoload 'penable-paredit-mode "paredit" "structural editing for lisps" t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook       #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook       #'rainbow-delimiters-mode)

;; swap return and C-j
;; (electric-indent-mode nil)


;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp)
	     (clojurec-mode . lsp)
	     (clojurescript-mode . lsp)
         (web-mode . lsp))
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (define-key lsp-mode-map (kbd "C-c l f") 'lsp-format-region)
  (define-key lsp-mode-map (kbd "C-c l F") 'lsp-format-buffer)
  (define-key lsp-mode-map (kbd "C-c l r") 'lsp-rename)
  (define-key lsp-mode-map (kbd "C-c l o") 'lsp-organize-imports)
  (define-key lsp-mode-map (kbd "C-c l a") 'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "C-c l d") 'lsp-ui-doc-glance)
  (define-key lsp-mode-map (kbd "C-c l p") 'lsp-ui-peek-find-references)
  (define-key lsp-mode-map (kbd "C-c l i") 'lsp-ui-imenu)

  ;; add paths to your local installation of project mgmt tools, like lein
  (setenv "PATH"
          (if (eq system-type 'darwin)
              (concat
		       "/opt/homebrew/bin" path-separator
		       (getenv "PATH"))
            (getenv "PATH")))
  (dolist (m '(clojure-mode
	           clojurec-mode
	           clojurescript-mode
	           clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

(setq lsp-headerline-breadcrumb-enable t)
(setq lsp-headerline-breadcrumb-enable-diagnostics nil)
(setq lsp-headerline-breadcrumb-segments '(project file symbols))

(setq lsp-enable-on-type-formatting nil)
(setf lsp-ui-sideline-show-code-actions nil)
(setq lsp-lens-enable nil)
(setq lsp-modeline-diagnostics-enable nil)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-show-with-mouse t)

;; disable lsp formatting, _really_ slow
(setq lsp-enable-indentation nil)
(setq lsp-enable-on-type-formatting nil)

;;; Clojure (cider)
(setq cider-save-file-on-load t)
(when (eq system-type 'darwin)
  (setq cider-lein-command "/opt/homebrew/bin/lein"))
(setq cider-repl-buffer-size-limit 100000)
(setq cider-use-xref t)
;; lowering precedence
(setq cider-xref-fn-depth 90)
(define-key cider-repl-mode-map (kbd "C-c C-o") 'cider-repl-clear-buffer)
(setq cider-repl-display-help-banner nil)

(global-set-key (kbd "C-c C-r C-r") 'xref-find-references)
(global-set-key [C-M-backspace] 'backward-kill-sexp)
;; jarchive
(jarchive-setup)

;; NO TABS
(setq-default indent-tabs-mode nil)

;; java
;; (require 'lsp-java)
;; (add-hook 'java-mode-hook #'lsp)

;; yaml files
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; templates
(add-to-list 'auto-mode-alist '("\\.md.template\\'" . markdown-mode))

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
;; (global-set-key "\t" 'company-complete-common)
(setq company-selection-wrap-around t)

;; Rainbow parens
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; imenu
(global-set-key (kbd "C-c i") 'counsel-imenu)

;; ts
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode))
