
;; Apple Macintosh specific configuration

;; use command key as meta
;(setq mac-command-modifier 'meta)

;; use aspell
(setq-default ispell-program-name "/opt/homebrew/bin/aspell")

;; need this to make the backspace key work in eat.
(with-eval-after-load 'eat
  (setq eat-term-name "xterm-256color"))

;(set-face-attribute 'default nil :family "Menlo" :height 140)
(set-face-attribute 'default nil :family "JetBrains Mono NL" :height 140 :weight 'medium)
;;(set-face-attribute 'default nil :family "Hack" :height 130)
;;(set-face-attribute 'default nil :family "Glass TTY VT220" :height 130)

