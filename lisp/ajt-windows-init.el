
;; Microsoft Windows specific configuration

;; slam line endings to dos
(defun ajt-dos ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos))

;; slam line endings to unix
(defun ajt-unix ()
  (interactive)
	(set-buffer-file-coding-system 'utf-8-unix))

;(set-face-attribute 'default nil :family "Courier" :height 110 :weight 'regular)
;(set-face-attribute 'default nil :family "Courier New" :height 110 :weight 'regular)
;(set-face-attribute 'default nil :family "Courier New" :height 110 :weight 'bold)
;(set-face-attribute 'default nil :family "Consolas" :height 120 :weight 'regular)
;(set-face-attribute 'default nil :family "JetBrains Mono" :height 110 :weight 'regular)
;(set-face-attribute 'default nil :family "Sudo" :height 140 :weight 'regular)
;(set-face-attribute 'default nil :family "Terminal" :height 160 :weight 'regular)
;(set-face-attribute 'default nil :family "Lucida Console" :height 130 :weight 'regular)
;(set-face-attribute 'default nil :family "JetBrains Mono" :height 90 :weight 'regular)
;(set-face-attribute 'default nil :family "Hack" :height 110 :weight 'regular)
;(set-face-attribute 'default nil :family "Source Code Pro" :height 110 :weight 'regular)
;; lul retro
;(set-face-attribute 'default nil :family "Glass TTY VT220" :height 120 :weight 'regular)


(set-face-attribute 'default nil :family "Consolas" :height 150 :weight 'regular)
;(set-face-attribute 'default nil :family "Ac437 IBM EGA 8x14" :height 160 :weight 'regular)


