;;
;; detects if buffer uses spaces or tabs for indenting and preseves that setting
;;

;; default offsets
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
(defvaralias 'typescript-indent-level 'tab-width)

(defun buffer-file-has-extension-p (&rest exts)
  "Check if the current buffer's file name ends with one of EXTS."
  (let ((file (buffer-file-name)))
    (when file
      (seq-some (lambda (ext) (string-suffix-p ext file)) exts))))

(defun ajt-tabs-modep()
  "determine if the current-buffer uses tabs or spaces for indentation"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((tab-count 0)
          (space-count 0)
          (line-count 0))
      (while (and (not (eobp)) (< line-count 1024))
        (let ((c (following-char)))
          (if (eq c 32)
              (setq space-count (1+ space-count))
            (if (eq c 10)
                (setq tab-count (1+ tab-count)))))
        (setq line-count (1+ line-count))
        (forward-line 1))
      (if (> tab-count space-count)
          't
        nil))))

(defun ajt-detect-and-set-indentation ()
  (interactive)
  (setq indent-tabs-mode (ajt-tabs-modep))
  (if (or (buffer-file-has-extension-p "js") (buffer-file-has-extension-p "ts"))
      (progn
        (setq-local tab-width 2))))
