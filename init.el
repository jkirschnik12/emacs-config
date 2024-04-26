;; packages
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)


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
  
  :hook
  ((clojure-mode . lsp)
   (clojurec-mode . lsp)
   (clojurescript-mode . lsp)
   (typescript-ts-mode . lsp)
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

(use-package paredit
  :diminish paredit-mode
  :config
  (defun my-paredit-delete ()
    "Intellij like delete where indentation is deleted when all chars 
     from bol to cursor are blank."
    (interactive)
    (if (string-match "^\s*$" (buffer-substring (pos-bol) (point)))
        (delete-indentation)
      (paredit-backward-delete)))
  :bind
  (:map paredit-mode-map
        ("DEL" . my-paredit-delete))
  :hook
  ((emacs-lisp-mode . enable-paredit-mode)
   (ielm-mode . enable-paredit-mode)
   (lisp-mode . enable-paredit-mode)
   (lisp-interaction-mode . enable-paredit-mode)
   (scheme-mode . enable-paredit-mode)
   (clojure-mode . enable-paredit-mode)
   (cider-repl-mode . enable-paredit-mode)))

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  )

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
  (setq cider-auto-select-error-buffer nil)
  (setq cider-auto-select-test-report-buffer nil)
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

  (defun clj-reload ()
    "Performs clojure reload."
    (interactive)
    ;; (cider-load-buffer)
    (let ((expr "(do
                 (require '[clj-reload.core :as reload])
                 (reload/reload)
                 (println \"FIN\"))"))
      (cider-interactive-eval expr
                              (cider-insert-eval-handler (cider-current-repl))
                              nil
                              (cider--nrepl-print-request-map 120))))
  :bind
  (("<f12>" . (lambda () (interactive) (cider-jack-in-universal 3)))
   (:map cider-mode-map
         ("<M-return>" . clerk-show)
         ("C-c C-l" . clj-reload))
   (:map cider-repl-mode-map
         ("C-c C-o" . cider-repl-clear-buffer)
         ("<C-return>" . paredit-RET)
         ("RET" . cider-repl-closing-return))))

(use-package clojure-mode
  :config
  (setq clojure-align-forms-automatically t))

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

(use-package counsel
  :diminish counsel-mode
  :bind
  (("C-c i" . counsel-imenu)
   ("M-x" . counsel-M-x)
   ("C-c k" . counsel-rg)))

(use-package sqlformat
  :requires sql
  :defines sql-mode-map
  :config
  (setq sqlformat-command 'pgformatter)
  ;; placeholder thing "-p'\B:[\w-]*/gm'"
  (setq sqlformat-args '("-s2" "-g" "-w120"))
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
   ("C-'" . avy-goto-word-1)))

(use-package hl-todo
  :config
  (global-hl-todo-mode)
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
  (setq ivy-wrap t)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (add-to-list 'ivy-ignore-buffers "\\*Messages\\*")
  (add-to-list 'ivy-ignore-buffers "\\*cider-test-report\\*")
  (add-to-list 'ivy-ignore-buffers "\\*Help\\*")
  (add-to-list 'ivy-ignore-buffers "\\*clojure-lsp\\*")
  (add-to-list 'ivy-ignore-buffers "\\*clojure-lsp::stderr\\*")
  (add-to-list 'ivy-ignore-buffers "\\*lsp-log\\*")
  (add-to-list 'ivy-ignore-buffers "\\*scratch\\*")
  ;; idk what this does
  (setcdr (assq t ivy-format-functions-alist) 'ivy-format-function-line)
  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  (defun ivy-switch-buffer-all ()
    (interactive)
    (let ((ivy-ignore-buffers '()))
      (ivy-switch-buffer)))
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-all)
   ("C-c v" . ivy-push-view)
   ("C-c V" . ivy-pop-view)))

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

(use-package exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "LSP_USE_PLISTS")
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

(use-package org-pomodoro
  :config
  (setq org-pomodoro-play-sounds nil)
  (setq org-pomodoro-manual-break t)

  :hook
  ((org-pomodoro-finished . (lambda ()
                              (raise-frame (selected-frame))
                              (momentary-string-display "Pomodoro Completed!" (point))))

   (org-pomodoro-overtime . (lambda ()
                              (raise-frame (selected-frame))
                              (momentary-string-display "Overtime started!" (point))))

   (org-pomodoro-short-break-finished . (lambda ()
                                          (raise-frame (selected-frame))
                                          (momentary-string-display "Short Break Completed!" (point))))

   (org-pomodoro-long-break-finished . (lambda ()
                                         (raise-frame (selected-frame))
                                         (momentary-string-display "Long Break Completed!" (point))))))

(use-package which-key
  :config
  (which-key-mode))

(use-package ob-clojure
  :ensure nil
  :requires (cider)
  :config
  (setq org-babel-clojure-backend 'cider))

(use-package org
  :ensure nil
  :defines org-babel-clojure-backend
  :config
  (setq org-confirm-babel-evaluate nil)
  (defun text-display-tweaks ()
    (visual-line-mode)
    (darkroom-mode)
    (display-line-numbers-mode 0))
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

;; idk
;; (setq gitlab-token "<REDACTED>")
;; (setq gl-base "https://gitlab.singlewire.lan/api/v4/")

;; (request (concat gl-base "merge_requests")
;;   :parser 'json-read
;;   :headers '(("Authorization" . (concat "Bearer " gitlab-token)))
;;   :success (cl-function
;;             (lambda (&key data &allow-other-keys)
;;               (message "Got:  %S" (request-response-data data))))
;;    :error (cl-function
;;             (lambda (&key data &allow-other-keys)
;;               (message "Got ERROR:  %S" data))))


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
   '(((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "jkirschnik-mpb")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(magit fish-mode ob-clojure org-pomodoro use-package lsp-java eglot adoc-mode typescript-mode jdecomp find-file-in-project diminish pkg-info simple-httpd jarchive darkroom ripgrep prettier-js rjsx-mode yaml-mode vc-msg jet flycheck-clj-kondo ivy-rich spaceline-all-the-icons multiple-cursors rainbow-delimiters paredit solarized-theme keypression git-gutter-fringe oauth2 counsel-projectile neotree company lsp-ivy lua-mode))
 '(tool-bar-mode nil))
