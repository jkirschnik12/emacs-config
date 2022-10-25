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

;; eglot
(add-hook 'clojure-mode-hook 'eglot-ensure)
(setq eglot-connect-timeout 60)

;;; Clojure
(setq cider-save-file-on-load t)
(setq cider-lein-command "/usr/bin/lein")
;; Uncomment for cider completion
;; (setq tab-always-indent 'complete)

