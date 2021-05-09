;; Frame appearance

(require 'init-arch)
(set-face-attribute 'default nil
		    :font (font-spec :name (cond (*on_macos* "Menlo")
						 (*on_linux* "Liberation Mono")
						 (t          "Courier") )
				     :size 13))

(provide 'init-frame)
