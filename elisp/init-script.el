;; Script formatted header text
(defun my-insert-script-header ()
  "Inserts scripting header"
  (interactive)
  (insert
   "#\n"
   "# Copyright " (format-time-string "%Y") " by Idiap Research Institute,"
   " http://www.idiap.ch\n"
   "#\n"
   "# See the file COPYING for the licence associated with this software.\n"
   "#\n"
   "# Author(s):\n"
   "#   " (user-full-name) ", " (format-time-string "%B %Y") "\n"
   "#\n"
   )
  )

;; My sh header.  c.f., the C header
(defun my-insert-sh-header ()
  "Inserts header for sh source"
  (interactive)
  (insert "#!/bin/sh\n")
  (my-insert-script-header)
  )

;; sh mode
(defun my-sh-mode-hook ()
  (setq tab-width 8)
  (setq sh-basic-offset 4)
  (setq indent-tabs-mode nil)
  (setq sh-indent-for-then 0)
  (setq sh-indent-for-do 0)
  (setq sh-indent-after-do '+)
  (setq sh-indent-for-case-label 0)
  (setq sh-indent-for-case-alt '+)
  (local-set-key [f5] 'my-insert-sh-header)
  )
(add-hook 'sh-mode-hook 'my-sh-mode-hook)

;; My perl header.  c.f., the C header
(defun my-insert-perl-header ()
  "Inserts header for perl source"
  (interactive)
  (insert "#!/usr/bin/perl -w\n")
  (my-insert-script-header)
  )

;; perl mode
(defun my-perl-mode-hook ()
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  (local-set-key [f5] 'my-insert-perl-header)
  )
(add-hook 'perl-mode-hook 'my-perl-mode-hook)

;; My python header.  c.f., the C header
(defun my-insert-python-header ()
  "Inserts header for python source"
  (interactive)
  (insert "#!/usr/bin/env python\n")
  (my-insert-script-header)
  )

;; python mode
(defun my-python-mode-hook ()
  (setq python-indent 4)
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  (local-set-key [f5] 'my-insert-python-header)
  )
(add-hook 'python-mode-hook 'my-python-mode-hook)

(setq auto-mode-alist (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SConscript" . python-mode) auto-mode-alist))

(provide 'init-script)
