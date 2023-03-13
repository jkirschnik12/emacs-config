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

(setq clojure-align-forms-automatically t)
