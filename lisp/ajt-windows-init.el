
;; Microsoft Windows specific configuration
(if (equal hostname "blackholesun")
    (set-face-attribute 'default nil :family "Hack" :height 90)
  (set-face-attribute 'default nil :family "Hack" :height 85)
