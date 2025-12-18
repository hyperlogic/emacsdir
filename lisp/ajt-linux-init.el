
;; GNU Linux specific configuration

;; for Ubuntu 200% scaling
(set-face-attribute 'default nil :family "JetBrainsMonoNL" :height 95 :weight 'light)

;; NOTE: use vterm-copy-mode to move the cursor like shell
(if is-linux-machine
    (use-package vterm :ensure t))







