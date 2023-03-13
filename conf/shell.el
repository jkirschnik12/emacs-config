(require 'eshell)

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (add-to-list 'exec-path "/opt/homebrew/bin/")
    (add-to-list 'exec-path "/usr/bin/"))

(add-hook 'eshell-mode-hook (lambda ()
			      (eshell/addpath "/opt/homebrew/bin/")
                              (eshell/addpath "/usr/bin/")))

;; just for eshell convenience
(defalias 'ff 'find-file)
(defalias 'de (lambda ()
		(cd "~/code/dev-environment")))
(defalias 'cl 'eshell/clear-scrollback)
(add-hook 'eshell-mode-hook (lambda ()
                              (display-line-numbers-mode 0)))

(require 'company)
(setq company-global-modes '(not eshell-mode))
