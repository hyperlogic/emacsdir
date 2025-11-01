;;
;; color theme
;;

(setq use-dark-theme 't)

;; built in emacs themes

;; light themes
;;(load-theme 'adwaita)
;;(load-theme 'dichromacy)
;;(load-theme 'leuven)
;;(load-theme 'light-blue)
;;(load-theme 'modus-operandi)
;;(load-theme 'tango)
;;(load-theme 'tsdh-light)
;;(load-theme 'whiteboard)

;; dark themes
;;(load-theme 'deeper-blue)
;;(load-theme 'manoj-dark)
;;(load-theme 'misterioso)
;;(load-theme 'modus-vivendi)
;;(load-theme 'tango-dark)
;;(load-theme 'tsdh-dark)
;;(load-theme 'wheatgrass)
;;(load-theme 'wombat)

(use-package doom-themes)

;; terminal dark themes
;;(load-theme 'doom-badger)
;;(load-theme 'doom-dark+)
;;(load-theme 'doom-gruvbox)
;;(load-theme 'doom-homage-black)
;;(load-theme 'doom-monokai-octagon)
;;(load-theme 'doom-nord-aurora)
;;(load-theme 'doom-nord)
;;(load-theme 'doom-nova)
;;(load-theme 'doom-opera)
;;(load-theme 'doom-spacegrey)
;;(load-theme 'doom-sourcerer)
;;(load-theme 'doom-tokyo-night)

(use-package zenburn-theme)


(setq zenburn-override-colors-alist
  '(("zenburn-fg" . "#E0E0D0")
    ("zenburn-bg" . "#181818")))

(if window-system
    (if use-dark-theme
        ;; window-system dark
        (progn
          (setq ajt-line-color "black")
          (load-theme 'doom-tokyo-night t))
      ;; window-system light
      (progn
        (setq ajt-line-color "light gray")
        (load-theme 'whiteboard)))
  (if use-dark-theme
      ;; terminal dark
      (progn
        (setq ajt-line-color "black")
        (load-theme 'doom-tokyo-night))
    ;; terminal light
    (progn
      (setq ajt-line-color "light gray")
      (load-theme 'whiteboard))))
