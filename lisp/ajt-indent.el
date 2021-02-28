;;
;; detects if code buffer uses spaces or tabs for indenting
;; and preseves that setting
;;

;; default offsets
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

(defun ajt-tabs-modep()
  "determine if the current-buffer uses tabs or spaces for indentation"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((tab-count 0)
	  (space-count 0))
      (while (not (eobp))
	(let ((c (following-char)))
	  (if (eq c 32)
	      (setq space-count (1+ space-count))
	    (if (eq c 10)
		(setq tab-count (1+ tab-count)))))
	(forward-line 1))
      (if (> tab-count space-count)
	  't
	nil))))

(defun ajt-detect-and-set-indentation ()
  (interactive)
  (setq indent-tabs-mode (ajt-tabs-modep)))
