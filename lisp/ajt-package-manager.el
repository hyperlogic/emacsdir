;;
;; package manager bullshit
;;
;; In theory, packages should automatically download when 'use-package' is encountered
;; in practice, it never works...
;;
;; If you receive the follwing error: (bad-signature "archive-contents.sig")
;; do the following:
;;  (setq package-check-signature nil)
;;  gpg --recv-keys 066DAFCB81E42C40
;; or:
;;  (setq package-check-signature nil)

;; sigh, due to various incompatiblities with emacs, msys2 & gnupg don't bother checking signuatures.
(if is-windows-machine
    (setq package-check-signature nil))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
(package-initialize)

;; Install `use-package` if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; automatically install packages if not present
(require 'use-package)
(setq use-package-always-ensure t)
