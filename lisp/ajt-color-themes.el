;;
;; color theme
;;

(setq use-dark-theme (not window-system))

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

(if window-system
    (if use-dark-theme
        ;; window-system dark
        (progn
          (setq ajt-line-color "black")
          (load-theme 'wombat))
      ;; window-system light
      (progn
        (setq ajt-line-color "light gray")
        (load-theme 'whiteboard)))
  (if use-dark-theme
      ;; terminal dark
      (progn
        (setq ajt-line-color "black")
        (load-theme 'wombat))
    ;; terminal light
    (progn
      (setq ajt-line-color "light gray")
      (load-theme 'whiteboard))))

(set-face-background hl-line-face ajt-line-color)
