
;; GNU Linux specific configuration

(set-face-attribute 'default nil :family "JetBrainsMonoNL" :height 120 :weight 'light)

;; NOTE: use vterm-copy-mode to move the cursor like shell
(if is-linux-machine
    (use-package vterm :ensure t))







