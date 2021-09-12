;; TeX and LaTeX setup
(require 'init-arch)
(setq bibtex-field-delimiters 'double-quotes)

;; My LaTeX footer
(defun my-insert-latex-footer ()
  "Inserts footer for LaTeX source"
  (interactive)
  (insert "%%% Local Variables:\n")
  (insert "%%% mode: latex\n")
  (insert "%%% TeX-master: t\n")
  (insert "%%% TeX-PDF-mode: t\n")
  (insert "%%% End:\n")
  )

;; My beamer slides
(defun my-insert-beamer-frame-1 ()
  "Insert a beamer frame, single column"
  (interactive)
  (insert
   "\\begin{frame}\n"
   "  \\frametitle{}\n"
   "  \\framesubtitle{}\n"
   "  \\begin{itemize}\n"
   "  \\item \n"
   "  \\end{itemize}\n"
   "\\end{frame}\n"
   )
  )

(defun my-insert-beamer-frame-2 ()
  "Insert a beamer frame, double column"
  (interactive)
  (insert
   "\\begin{frame}\n"
   "  \\frametitle{}\n"
   "  \\framesubtitle{}\n"
   "  \\begin{columns}\n"
   "    \\column{0.5\\columnwidth}\n"
   "    \\begin{itemize}\n"
   "    \\item \n"
   "    \\end{itemize}\n"
   "    \\column{0.5\\columnwidth}\n"
   "    \\begin{itemize}\n"
   "    \\item \n"
   "    \\end{itemize}\n"
   "  \\end{columns}\n"
   "\\end{frame}\n"
   )
  )

;; AucteX
(load "auctex.el" nil t t)
;(load "preview-latex.el" nil t t)
(defun my-TeX-mode-hook ()
  (setq-default TeX-PDF-mode t)
  (when *on_linux*
    (setq-default TeX-view-program-selection (quote ((output-pdf "xdg-open")))))
  (local-set-key [f5] 'my-insert-latex-footer)
  (local-set-key [f6] 'my-insert-beamer-frame-1)
  (local-set-key [f7] 'my-insert-beamer-frame-2)
  )
(add-hook 'TeX-mode-hook 'my-TeX-mode-hook)

