
;; Microsoft Windows specific configuration

(if (not (equal hostname "blackholesun"))
    (progn
      (set-face-attribute 'default nil :family "Consolas" :height 90 :weight 'regular)
      (setq-default ispell-program-name "aspell"))
  ;;(set-face-attribute 'default nil :family "Hack" :height 110)
  (set-face-attribute 'default nil :family "InputMono" :height 10))
