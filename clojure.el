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

(setq clojure-align-forms-automatically t)
