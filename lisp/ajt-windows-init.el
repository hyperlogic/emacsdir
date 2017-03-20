
;; Microsoft Windows specific configuration

(if (not (equal hostname "blackholesun"))
    (set-face-attribute 'default nil :family "Consolas" :height 110 :weight 'regular)
  (set-face-attribute 'default nil :family "Hack" :height 110))


