;; packages
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(setenv "LSP_USE_PLISTS" "true")

;; This is only needed once, near the top of the file
(eval-when-compile
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(require 'diminish)
(require 'bind-key)

;; Start emacs server
(use-package server
  :config (if (and (fboundp 'server-running-p)
                   (not (server-running-p)))
              (server-start)))

;; mac modifiers
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'control
        mac-control-modifier 'super))

;; etc keybindings and settings
(global-set-key (kbd "C-c C-r C-r") 'xref-find-references)
(global-set-key [C-M-backspace] 'backward-kill-sexp)
(global-set-key (kbd "s-i") 'vc-msg-show)
(global-set-key (kbd "C-x j") 'eshell)

;; performance for lsp
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold #x40000000)
(add-function :after
              after-focus-change-function
              (lambda () (unless (frame-focus-state)
                           (garbage-collect))))

;;tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))



(setq-default indent-tabs-mode nil)

(use-package exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "LSP_USE_PLISTS")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package kkp
  :ensure t
  :config
  (setq kkp-control-modifier 'super)
  (setq kkp-super-modifier 'control)
  ;; (setq kkp-alt-modifier 'alt) ;; use this if you want to map the Alt keyboard modifier to Alt in Emacs (and not to Meta)
  (global-kkp-mode +1))

(use-package flycheck
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  ;; disable unnecessary flycheck stuff in elisp
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode)
  (use-package flycheck-clj-kondo))

(use-package which-key
  :diminish which-key-mode
  
  :config
  (which-key-mode))

(use-package ace-window
  :config
  (setq aw-keys '(?j ?k ?l ?\; ?a ?s ?d ?f))
  :bind
  (("M-`" . ace-swap-window)
   ("C-x C-o" . ace-window)
   ("C-x o" . ace-window)))

(use-package lsp-mode
  :defines (lsp-headerline-breadcrumb-enable-diagnostics lsp-headerline-breadcrumb-segments)

  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-auto-execute-action nil)
  
  :hook
  ((clojure-mode . lsp)
   (clojure-ts-mode . lsp)
   (clojurec-mode . lsp)
   (clojurescript-mode . lsp)
   (typescript-ts-mode . lsp)
   (tsx-ts-mode . lsp)
   (java-mode . lsp)
   (typescript-mode . lsp))
  
  :bind
  (:map lsp-mode-map
        ("C-c l f" . lsp-format-region)
        ("C-c l F" . lsp-format-buffer)
        ("C-c l r" . lsp-rename)
        ("C-c l o" . lsp-organize-imports)
        ("C-c l a" . lsp-execute-code-action))
  :init
  (setenv "LSP_USE_PLISTS" "true")
  :config 
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-lens-enable nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-use-plists t)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setenv "PATH"
          (if (eq system-type 'darwin)
              (concat
	       "/opt/homebrew/bin" path-separator
	       (getenv "PATH"))
            (getenv "PATH")))
  (dolist (m '(clojure-mode
               clojure-ts-mode
	       clojurec-mode
	       clojurescript-mode
	       clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure")))
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-headerline-breadcrumb-segments '(project file symbols)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind
  (:map lsp-ui-mode-map
        ("C-c l d" . lsp-ui-doc-glance)
        ("C-c l p" . lsp-ui-peek-find-references)
        ("C-c l i" . lsp-ui-imenu))
  :config
  (setf lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-mouse t))

(use-package lsp-java
  :config
  ;; (setenv "JAVA_HOME"  "/Users/joe.kirschnik/.asdf/shims/java/Contents/Home/")
  ;; (setq lsp-java-java-path "/Users/joe.kirschnik/.asdf/shims/java/Contents/Home/bin/java")
  )


(use-package smartparens
  :demand t
  :ensure smartparens  ;; install the package
  :bind
  (:map smartparens-mode-map
        ("C-k" . sp-kill-hybrid-sexp)
        ("C-M-k" . sp-kill-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("M-s" . sp-splice-sexp)
        ("C-<right>" . sp-forward-slurp-sexp)
        ("C-<left>" . sp-forward-barf-sexp)
        ("C-M-<right>" . sp-backward-slurp-sexp)
        ("C-M-<left>" . sp-backward-barf-sexp)
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("DEL" . sp-backward-delete-char)
        ("M-f" . sp-forward-symbol)
        ("M-b" . sp-backward-symbol))
  :config
  (smartparens-global-mode)
  (smartparens-global-strict-mode -1)
  (sp-local-pair '(clojure-ts-mode clojure-mode emacs-lisp-mode lisp-mode cider-repl-mode)
                 '"'" nil :actions nil))

(use-package typescript-mode
  :demand t)

(use-package prettier-js
  :hook ((typescript-ts-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode)
         (tsx-ts-mode . prettier-js-mode)))

(use-package cider
  :functions (cider-interactive-eval
              cirder-current-repl
              cider-insert-eval-handler)
  :config
  (setq cider-save-file-on-load t)
  (when (eq system-type 'darwin)
    (setq cider-lein-command "/opt/homebrew/bin/lein"))
  (setq cider-repl-buffer-size-limit 100000)
  (setq cider-use-xref t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-auto-select-test-report-buffer t)
  (setq cider-xref-fn-depth 90)
  (setq cider-repl-display-help-banner nil)
  (setq cider-interactive-eval-output-destination 'repl-buffer)
  (defun clerk-show ()
    "Open file in clerk."
    (interactive)
    (when-let
        ((filename
          (buffer-file-name)))
      (save-buffer)
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")"))))
  (setq cider-ns-code-reload-tool 'clj-reload)
  :bind
  (("<f12>" . (lambda () (interactive) (cider-jack-in-universal 3)))
   (:map cider-mode-map
         ("<M-return>" . clerk-show)
         ("C-c C-l" . clj-reload))
   (:map cider-repl-mode-map
         ("C-c C-o" . cider-repl-clear-buffer)
         ("<C-return>" . sp-newline)
         ("RET" . cider-repl-closing-return))))

(use-package clojure-mode
  :config
  (setq clojure-align-forms-automatically t))

;; (use-package clojure-ts-mode)

(use-package eldoc
  :diminish eldoc-mode
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  :hook
  ((emacs-lisp-mode . turn-on-eldoc-mode)
   (lisp-interaction-mode . turn-on-eldoc-mode)
   (ielm-mode . turn-on-eldoc-mode)
   (cider-mode . turn-on-eldoc-mode)))

(use-package jarchive
  :config
  (jarchive-mode))

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  ;; templates
  (add-to-list 'auto-mode-alist '("\\.md.template\\'" . markdown-mode)))

(use-package company
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("M-/" . company-complete))
  :config
  (setq company-selection-wrap-around t)
  (setq company-global-modes '(not eshell-mode org-mode)))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package sqlformat
  :demand t
  :config
  (setq sqlformat-command 'pgformatter)
  ;; placeholder thing "-p'\B:[\w-]*/gm'"
  (setq sqlformat-args '("-s2" "-g" "-w120"))
  (require 'sql)
  :bind
  (:map sql-mode-map
        ("C-c l f" . sqlformat)
        ("C-c l F" . sqlformat-buffer)))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("<mouse-2>" . mc/add-cursor-on-click)))

(use-package avy
  :config
  (setq avy-all-windows nil)
  :bind
  (("M-g l" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("C-'" . avy-goto-char-2)
   ("C-\"" . avy-goto-char-timer)))

(use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode 1)
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#ffe700")
          ("todo"   . "#ffe700")
          ("FIXME"  . "#FF0000")
          ("ask" . "#09e237")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF")
          ("stub"   . "#1E90FF")))
  :bind
  (:map hl-todo-mode-map
        ("C-c ; p" . hl-todo-previous)
        ("C-c ; n" . hl-todo-next)))

(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode +1)
  (defun my-next-git-gutter-diff (arg)
    (interactive "p")
    (git-gutter:next-hunk arg)
    (recenter))
  (defun my-prev-git-gutter-diff (arg)
    (interactive "p")
    (git-gutter:previous-hunk arg)
    (recenter))
  :bind
  (("C-M-3" . my-next-git-gutter-diff)
   ("C-M-2" . my-prev-git-gutter-diff)
   ("C-c g d" . git-gutter:popup-hunk)
   ("C-c g r" . git-gutter:revert-hunk)
   ("C-c g SPC" . git-gutter:mark-hunk)))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package autorevert
  :ensure nil
  :diminish auto-revert-mode)

(use-package ivy
  :diminish ivy-mode 
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-height 10)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  ;; idk what this does
  (setcdr (assq t ivy-format-functions-alist) 'ivy-format-function-line)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus))))

(use-package counsel
  :diminish counsel-mode
  :config
  (counsel-mode 1)
  :bind
  (("C-c k" . counsel-rg)
   ("C-x C-f" . counsel-find-file)
   ("C-x b" . counsel-switch-buffer)
   ("C-c i" . counsel-imenu)))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

(use-package swiper
  :bind
  (("C-s" . swiper)
   ("C-r" . swiper)
   ("C-M-s" . swiper-thing-at-point)))

(use-package fish-mode)
(use-package counsel-projectile)

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  )

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (setq projectile-sort-order 'recentf)
  (setq projectile-create-missing-test-files t)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)
        ("C-c p s" . counsel-projectile-rg)))

(add-to-list 'load-path "~/.emacs.d/misc/asdf.el")
(require 'asdf)

(asdf-enable) ;; This ensures Emacs has the correct paths to asdf shims and bin


(use-package esh-mode
  :ensure nil
  :functions (eshell-send-input run-cmd-in-eshell)
  :config
  (defalias 'ff 'find-file)
  (defalias 'ms 'magit-status)
  (defalias 'cl 'eshell/clear-scrollback)
  (defun run-cmd-in-eshell (cmd)
    "Runs the command cmd in eshell."
    (with-current-buffer "*eshell*"
      (goto-char (point-max))
      (insert cmd)
      (eshell-send-input)))
  (when (memq window-system '(mac ns x))
    (add-to-list 'exec-path "/opt/homebrew/bin/")
    (add-to-list 'exec-path "/usr/bin/")
    (defalias 'de (lambda ()
		    (cd "~/code/dev-environment")))
    (defalias 'aw (lambda ()
		    (cd "~/code/dev-environment/admin-webapp")))
    (defalias 'ci (lambda ()
		    (cd "~/code/dev-environment/cirrus-rest-api")))
    (defalias 'rn (lambda ()
		    (cd "~/code/dev-environment/icmobile-react-native")))
    (setenv "DEV_ENVIRONMENT_ROOT" "/Users/joe.kirschnik/code/dev-environment")
    (defalias 'cb (lambda ()
                    (cd "~/code/dev-environment")
                    (eshell-command "bin/checkout-branches.clj" t)))
    (add-hook 'eshell-mode-hook (lambda ()
			          (eshell/addpath "/opt/homebrew/bin/")
                                  (eshell/addpath "/usr/bin/"))))
  :bind
  (:map eshell-mode-map
        ("C-c C-o" . (lambda ()
                       (interactive)
                       (run-cmd-in-eshell "clear-scrollback")))))

(use-package which-key
  :config
  (which-key-mode))

(use-package ob-clojure
  :ensure nil
  :requires (cider)
  :config
  (setq org-babel-clojure-backend 'cider))

(use-package verb)

(use-package org
  :ensure nil
  :defines org-babel-clojure-backend
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (require 'org-tempo)
  (setq org-confirm-babel-evaluate nil)
  (defun text-display-tweaks ()
    (let* ((buf (current-buffer))
           (name (buffer-file-name buf)))
      (when (not (string-prefix-p "/Users/joe.kirschnik/reqs" name))
        (visual-line-mode)
        (darkroom-mode)
        (display-line-numbers-mode 0))))
  :hook
  (org-mode . text-display-tweaks))

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

(setq counsel-linux-app-format-function
      'counsel-linux-app-format-function-name-pretty)

(defun emacs-run-launcher ()
  "Create and select a frame called emacs-run-launcher which 
   consists only of a minibuffer and has specific dimensions. 
   Run counsel-linux-app on that frame,which is an emacs 
   command that prompts you to select an app and open it in a 
   dmenu like behaviour. Delete the frame after that command has exited"
  (interactive)
  (with-selected-frame
      (make-frame '((name . "emacs-run-launcher")
                    (minibuffer . only)
                    (width . 120)
                    (height . 11)
                    (fullscreen . 0)))
    (unwind-protect
        (counsel-linux-app)
      (delete-frame))))

(use-package magit
  :config
  (transient-append-suffix 'magit-push "-u"
    '(1 "=s" "Skip gitlab pipeline" "--push-option=ci.skip"))
  (transient-append-suffix 'magit-commit "-C"
    '(1 "=n" "no verify" "--no-verify"))
  )
(use-package s)
(use-package lsp-ivy)
(use-package darkroom)
(use-package jet)
(use-package jdecomp)
(use-package dap-mode)

(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/extra.el")
(when (memq window-system '(mac ns x))
  (load "~/.emacs.d/sw.el"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(adoc-code-face ((t (:inherit fixed-pitch :foreground "orange2"))))
 '(adoc-gen-face ((t (:foreground "plum3"))))
 '(adoc-meta-face ((t (:stipple nil :foreground "LightSkyBlue1" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant italic :weight normal :width normal :foundry "unknown" :family "Monospace"))))
 '(adoc-meta-hide-face ((t (:inherit adoc-meta-face :foreground "gray65"))))
 '(adoc-verbatim-face ((t nil)))
 '(font-lock-comment-face ((t (:foreground "thistle4" :slant italic)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "flatpak") tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp :machine "localhost") tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "jkirschnik-mpb") tramp-connection-local-darwin-ps-profile)
     ((:application tramp) tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin"
                         "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin"
                         "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number) (user . string) (egid . number) (comm . 52)
                                          (state . 5) (ppid . number) (pgrp . number) (sess . number) (ttname . string)
                                          (tpgid . number) (minflt . number) (majflt . number) (time . tramp-ps-time)
                                          (pri . number) (nice . number) (vsize . number) (rss . number)
                                          (etime . tramp-ps-time) (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number) (user . string) (group . string) (comm . 52) (state . 5)
                                          (ppid . number) (pgrp . number) (ttname . string) (time . tramp-ps-time)
                                          (nice . number) (etime . tramp-ps-time) (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number) (user . string) (egid . number)
                                          (group . string) (comm . 52) (state . string) (ppid . number) (pgrp . number)
                                          (sess . number) (ttname . string) (tpgid . number) (minflt . number)
                                          (majflt . number) (time . tramp-ps-time) (pri . number) (nice . number)
                                          (vsize . number) (rss . number) (etime . number) (pcpu . number)
                                          (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile (shell-file-name . "/bin/sh") (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile (path-separator . ":") (null-device . "/dev/null"))))
 '(gitlab-lsp-major-modes
   '(python-mode python-ts-mode go-mode go-ts-mode js-mode js-ts-mode java-mode java-ts-mode kotlin-mode kotlin-ts-mode
                 ruby-mode ruby-ts-mode rust-mode rust-ts-mode tsx-ts-mode typescript-mode typescript-ts-mode vue-mode
                 yaml-mode yaml-ts-mode clojure-mode))
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(adoc-mode all-the-icons ascii-art-to-unicode cider color-theme-sanityinc-tomorrow counsel-projectile darkroom diminish
               dirvish eglot exec-path-from-shell find-file-in-project fish-mode flycheck-clj-kondo git-gutter-fringe
               gitlab-lsp hl-todo ivy-rich jarchive jdecomp jet js2-mode keypression kkp kotlin-ts-mode logview lsp-ivy
               lsp-java lsp-ui lua-mode magit memoize multiple-cursors oauth2 paredit pkg-info prettier-js
               quelpa-use-package rainbow-delimiters ripgrep rust-mode sideline simple-httpd smartparens spaceline
               sqlformat tree-sitter-langs typescript-mode vc-msg verb web-mode which-key xpm yaml-mode yasnippet
               yasnippet-snippets))
 '(tool-bar-mode nil))
(put 'magit-clean 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
