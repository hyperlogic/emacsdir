;;
;; Refresh the compilation-mode on the ajt-grep buffer
;;
(defun ajt-refresh-compilation-mode-on-grep (process event)
  (let ((prev-buffer (current-buffer)))
    (switch-to-buffer "*ajt-grep*")

    ;; bit of a hack, for windows using msys2
    (when is-windows-machine
      (let ((case-fold-search nil))
        (setq buffer-read-only nil)
        (goto-char (point-min))
        (while (re-search-forward (concat "^/home/" username) nil t)
          (replace-match (concat "c:/msys64/home/" username)))))

    (end-of-buffer)
    (setq buffer-read-only nil)
    (insert (format "\nProcess %s %s\n" process event))
    (setq buffer-read-only 't)
    (compilation-mode)
    (switch-to-buffer prev-buffer)))

;;
;; Pops up a grep process in a buffer named *ajt-grep*
;;
(defun ajt-grep-find-shell-cmd (cmd)
  ;; echo the cmd line to the *Message* log
  (message cmd)

  ;; exec command in a new process
  (let ((process (start-process-shell-command "ajt-grep" "*ajt-grep*" cmd))
        (buffer (pop-to-buffer "*ajt-grep*")))
    (set-process-sentinel process 'ajt-refresh-compilation-mode-on-grep)
    (compilation-mode)))

(defun ajt-filter-path-patterns (patterns)
  (remq nil (mapcar (lambda (x) (if (string-equal (substring x 0 1) "!") nil (concat "\"" x "\""))) patterns)))

(defun ajt-filter-include-patterns (patterns)
  (remq nil (mapcar (lambda (x) (if (string-equal (substring x 0 1) "!") nil x)) patterns)))

(defun ajt-concat-include-patterns (type patterns)
  (let ((include-patterns (ajt-filter-include-patterns patterns)))
    (if include-patterns
        (concat "\\( " (mapconcat (lambda (x) (concat type " \"" x "\"")) include-patterns " -or ") " \\) ")
      "")))

(defun ajt-filter-exclude-patterns (patterns)
  (remq nil (mapcar (lambda (x) (if (string-equal (substring x 0 1) "!") (substring x 1) nil)) patterns)))

(defun ajt-concat-exclude-patterns (type patterns)
  (let ((exclude-patterns (ajt-filter-exclude-patterns patterns)))
    (if exclude-patterns
        (concat "-not \\( " (mapconcat (lambda (x) (concat type " \"" x "\"")) exclude-patterns " -or ") " \\) ")
      "")))

;;
;; Custom grep search
;;
(defun ajt-grep-find (search-term path-patterns name-patterns)
  "Passes the string SEARCH-TERM to grep
SEARCH-TERM regex to search for (passed to grep)
PATH-PATTERNS is a list of find style patterns that must match full path.  If pattern starts with a ! character then it can be used to exclude a path.
NAME-PATTERNS is a list of find style patterns that must match base filename.  If pattern starts with a ! character then it can be used to exclude a file.
For example:
  (ajt-grep-find \"main\" '(\"d:/tras/cdc/runtime\" \"d:/tras/code/game\") '(\"*.cpp\" \"*.h\" \"*.c\")))"

  (let* ((path-include-string (mapconcat 'identity (ajt-filter-path-patterns path-patterns) " "))
         (path-exclude-string (ajt-concat-exclude-patterns "-path" path-patterns))
         (name-include-string (ajt-concat-include-patterns "-name" name-patterns))
         (name-exclude-string (ajt-concat-exclude-patterns "-name" name-patterns))
         (cmd (make-string 0 ?x)))

    (setq cmd (concat cmd "find " path-include-string))
    (when (not (string-equal path-exclude-string ""))
      (setq cmd (concat cmd " " path-exclude-string)))
    (when (not (string-equal name-include-string ""))
      (setq cmd (concat cmd " " name-include-string))) ; hack for windows
    (when (not (string-equal name-exclude-string ""))
      (setq cmd (concat cmd " -and " name-exclude-string)))

    (if is-windows-machine
        (setq cmd (concat cmd " -type f -print0 | \"xargs\" -0 -e grep -nH -e " search-term))
      (setq cmd (concat cmd " -type f -exec grep -nH -e " search-term " {} /dev/null \\;")))

    ;; send cmd to *Messages*
    (message cmd)

    ;(message (format "path-include-patterns = %S" path-include-string))
    ;(message (format "path-exclude-patterns = %S" path-exclude-string))
    ;(message (format "name-include-patterns = %S" name-include-string))
    ;(message (format "name-exclude-patterns = %S" name-exclude-string))

    (ajt-grep-find-shell-cmd cmd)))
