;; Constants for the architecture
;; Basically from cabins' repo, but looking like homebrew

(defconst *on_macos* (eq system-type 'darwin)
  "Arch is MacOS")

(defconst *on_linux* (eq system-type 'gnu/linux)
  "Arch is GNU/Linux")

(defconst *on_windows* (or (eq system-type 'ms-dos)
			   (eq system-type 'windows-nt))
  "Arch is Windows or DOS")

(provide 'init-arch)
