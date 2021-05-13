;; Ruby mode

;; My ruby header.  c.f., the C header
(defun my-insert-ruby-header ()
  "Inserts header for ruby source"
  (interactive)
  (insert "#!/usr/bin/ruby\n")
  (my-insert-script-header)
  )

;; ruby mode
(defun my-ruby-mode-hook ()
  (local-set-key [f5] 'my-insert-ruby-header)
  )
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(provide 'init-ruby)
