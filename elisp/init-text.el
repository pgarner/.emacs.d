;; Text modes
(defun my-text-mode-hook ()
  (visual-line-mode 1)
  (ispell-change-dictionary "british")
  (flyspell-mode t)
  )
(add-hook 'text-mode-hook 'my-text-mode-hook)

(provide 'init-text)
