(require 'cider)
(require 'clojure-mode)

(defun clerk-show ()
  "Open file in clerk."
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(define-key cider-mode-map (kbd "<M-return>") 'clerk-show)
(define-key cider-mode-map (kbd "C-c c f") 'cider-format-defun)
(define-key cider-mode-map (kbd "C-c c F") 'cider-format-region)
(define-key cider-mode-map (kbd "C-c c C-f") 'cider-format-buffer)

(define-key cider-repl-mode-map (kbd "<C-return>") 'paredit-RET)
(define-key cider-repl-mode-map (kbd "RET") 'cider-repl-closing-return)

(setq clojure-align-forms-automatically t)

;; https://docs.cider.mx/cider/basics/up_and_running.html#universal-jack-in
;; quick babashka repl
(global-set-key (kbd "<f12>") (lambda ()
                                (interactive)
                                (cider-jack-in-universal 3)))
