(require 'clojure-mode)
(require 'eglot)
(require 'cider)
(require 'eldoc)
(require 'use-package)
(require 'lsp-ui)
(require 'lsp)

(autoload 'penable-paredit-mode "paredit" "structural editting for lisps" t)
(add-hook 'emacs-list-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook       #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook       #'rainbow-delimiters-mode)

;; swap ret and C-j
(electric-indent-mode)

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :hook ((clojure-mode . lsp)
	 (clojurec-mode . lsp)
	 (clojurescript-mode . lsp))
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
  (setenv "PATH" (concat
		  "/opt/homebrew/bin" path-separator
		  (getenv "PATH")))
  (dolist (m '(clojure-mode
	       clojurec-mode
	       clojurescript-mode
	       clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
   ;; (setq lsp-clojure-server-command '("/opt/homebrew/bin/clojure-lsp"))
   )
;; (setq lsp-typescript-npm '("/opt/homebrew/bin/npm"))


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
;; this got annoying
(setq lsp-ui-doc-show-with-mouse nil)
;; (setq lsp-ui-doc-show-with-cursor nil)

;; disable lsp formatting, _really_ slow
(setq lsp-enable-indentation nil)
(setq lsp-enable-on-type-formatting nil)

;;; Clojure (cider)
(setq cider-save-file-on-load t)
(setq cider-lein-command "/opt/homebrew/bin/lein")
(setq cider-repl-buffer-size-limit 100000)
(setq cider-use-xref t)
;; lowering precedence
(setq cider-xref-fn-depth 90)
(define-key cider-repl-mode-map (kbd "C-c C-o") 'cider-repl-clear-buffer)
(define-key cider-mode-map (kbd "C-c C-o") 'cider-repl-clear-buffer)
(setq cider-repl-display-help-banner nil)
;; (setq tab-always-indent 'complete)

(global-set-key (kbd "C-c C-r C-r") 'xref-find-references)

;; jarchive
(jarchive-setup)
