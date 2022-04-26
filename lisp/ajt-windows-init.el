
;; Microsoft Windows specific configuration

(if (not (equal hostname "blackholesun"))
    (progn
      ;(set-face-attribute 'default nil :family "Courier" :height 110 :weight 'regular)
      ;(set-face-attribute 'default nil :family "Courier New" :height 110 :weight 'regular)
      ;(set-face-attribute 'default nil :family "Courier New" :height 110 :weight 'bold)
      (set-face-attribute 'default nil :family "Consolas" :height 110 :weight 'regular)
      ;(set-face-attribute 'default nil :family "JetBrains Mono" :height 110 :weight 'regular)
      ;(set-face-attribute 'default nil :family "Sudo" :height 140 :weight 'regular)
      ;(set-face-attribute 'default nil :family "Terminal" :height 160 :weight 'regular)
      ;(set-face-attribute 'default nil :family "Lucida Console" :height 130 :weight 'regular)
      ;(set-face-attribute 'default nil :family "JetBrains Mono" :height 90 :weight 'regular)
      ;(set-face-attribute 'default nil :family "Hack" :height 110 :weight 'regular)
      ;(set-face-attribute 'default nil :family "Source Code Pro" :height 110 :weight 'regular)
      ;; lul retro
      ;(set-face-attribute 'default nil :family "Glass TTY VT220" :height 120 :weight 'regular)
      (setq-default ispell-program-name "aspell"))
  ;;(set-face-attribute 'default nil :family "Hack" :height 110)
  (set-face-attribute 'default nil :family "InputMono" :height 100))
