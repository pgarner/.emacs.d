;; Frame appearance

; Actually, this is moot.  The init files get read after the first window is
; created, so it's better to set fonts using .Xresources

(require 'init-arch)
(set-face-attribute 'default nil
		    :font (font-spec :name (cond (*on_macos* "Menlo")
						 (*on_linux* "Liberation Mono")
						 (t          "Courier") )
				     :size 13))

(provide 'init-frame)
