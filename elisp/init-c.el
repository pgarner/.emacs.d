;; My C header
;; 'man strftime' for the time string codes
(defun my-insert-c-header ()
  "Inserts header for C source"
  (interactive)
  (insert
   "/*\n"
   " * Copyright " (format-time-string "%Y") " by Idiap Research Institute,"
   " http://www.idiap.ch\n"
   " *\n"
   " * See the file COPYING for the licence associated with this software.\n"
   " *\n"
   " * Author(s):\n"
   " *   " (user-full-name) ", " (format-time-string "%B %Y") "\n"
   " */\n"
   )
  )

;; .h files tend to be C++ rather than C
(setq auto-mode-alist (append '(("\\.h$" . c++-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.cu$" . c++-mode)) auto-mode-alist))

;; C and C++ mode hook
;(require 'google-c-style)
(defun c-mode-common-hook-bsd ()
  (c-set-style "BSD")
  (setq tab-width 8)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (c-set-offset 'arglist-close 0)
  (local-set-key [f5] 'my-insert-c-header)
  )
(defun c-mode-common-hook-google ()
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  (google-set-c-style)
  (local-set-key [f5] 'my-insert-c-header)
  )
(add-hook 'c-mode-common-hook 'c-mode-common-hook-bsd)
;(add-hook 'c-mode-common-hook 'c-mode-common-hook-google)

(provide 'init-c)
