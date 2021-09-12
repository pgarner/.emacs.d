;;
;; Emacs setup
;; Phil Garner, April 2007
;;

;; My load path
(add-to-list 'load-path
	     (expand-file-name (concat user-emacs-directory "elisp/")))

;; Basic interface
(mwheel-install)
(setq make-backup-files nil)
(setq-default fill-column 79)
(delete-selection-mode t)
(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; This points HomeBrew's cask emacs at where the packages get installed
(let ((default-directory  "/usr/local/share/emacs/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Stop those semantic-cache files
(setq semanticdb-default-save-directory "~/.semanticdb")

(require 'init-arch)
(require 'init-frame)
(require 'init-ruby)
(require 'init-tex)

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

;; My perl header.  c.f., the C header
(defun my-insert-perl-header ()
  "Inserts header for perl source"
  (interactive)
  (insert "#!/usr/bin/perl -w\n")
  (my-insert-script-header)
  )

;; My python header.  c.f., the C header
(defun my-insert-python-header ()
  "Inserts header for python source"
  (interactive)
  (insert "#!/usr/bin/env python\n")
  (my-insert-script-header)
  )

;; .h files tend to be C++ rather than C
(setq auto-mode-alist (append '(("\\.h$" . c++-mode)) auto-mode-alist))

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

;; perl mode
(defun my-perl-mode-hook ()
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  (local-set-key [f5] 'my-insert-perl-header)
  )
(add-hook 'perl-mode-hook 'my-perl-mode-hook)

;; python mode
(defun my-python-mode-hook ()
  (setq python-indent 4)
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  (local-set-key [f5] 'my-insert-python-header)
  )
(add-hook 'python-mode-hook 'my-python-mode-hook)

;; Text modes
(defun my-text-mode-hook ()
  (visual-line-mode 1)
  (ispell-change-dictionary "british")
  (flyspell-mode t)
  )
(add-hook 'text-mode-hook 'my-text-mode-hook)

;; Gnuplot
(autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
(autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot mode" t)
(setq auto-mode-alist (append '(("\\.gp$" . gnuplot-mode)) auto-mode-alist))

;; PKGBUILD
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

;; CMake
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

;; Lua mode
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; C-sharp
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(defun my-csharp-mode-hook ()
  "function that runs when csharp-mode is initialized for a buffer.")
(add-hook  'csharp-mode-hook 'my-csharp-mode-hook t)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "latin-1-prefix")
 '(font-latex-fontify-sectioning (quote color))
 '(global-font-lock-mode t nil (font-lock))
 '(load-home-init-file t t)
 '(safe-local-variable-values (quote ((TeX-master . tracter))))
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil))
