
;; Microsoft Windows specific configuration

(if (equal hostname "blackholesun")
    (set-face-attribute 'default nil :family "Consolas" :height 120 :weight 'regular)
  (set-face-attribute 'default nil :family "Hack" :height 90))

