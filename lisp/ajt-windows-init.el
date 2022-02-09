
;; Microsoft Windows specific configuration

(if (not (equal hostname "blackholesun"))
    (progn
      (set-face-attribute 'default nil :family "Consolas" :height 100 :weight 'regular)
      ;(set-face-attribute 'default nil :family "Hack" :height 95 :weight 'regular)
      ;(set-face-attribute 'default nil :family "Source Code Pro" :height 95 :weight 'regular)
      ;; lul retro
      ;(set-face-attribute 'default nil :family "Glass TTY VT220" :height 100 :weight 'regular)
      (setq-default ispell-program-name "aspell"))
  ;;(set-face-attribute 'default nil :family "Hack" :height 110)
  (set-face-attribute 'default nil :family "InputMono" :height 100))
