
;; Microsoft Windows specific configuration

;; slam line endings to dos
(defun ajt-dos ()
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos))

;; slam line endings to unix
(defun ajt-unix ()
  (interactive)
	(set-buffer-file-coding-system 'utf-8-unix))


;; Add emoji font as fallback for unicode symbols only
(set-fontset-font t 'unicode "Segoe UI Emoji" nil 'prepend)

(set-frame-font "JetBrains Mono NL Regular-12")

