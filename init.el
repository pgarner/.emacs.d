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
;; (cua-mode t)
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
(require 'init-c)
(require 'init-script)
(require 'init-text)

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
 '(font-latex-fontify-sectioning 'color)
 '(font-use-system-font t)
 '(global-font-lock-mode t nil (font-lock))
 '(load-home-init-file t t)
 '(safe-local-variable-values '((TeX-master . tracter)))
 '(show-paren-mode t nil (paren))
 '(tool-bar-mode nil))
