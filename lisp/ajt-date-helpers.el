;; converts the unix epoc timestamp under the cursor
;; to a human readable date by using the date command
(defun ajt-timestamp-to-date ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds))
         (timestamp (buffer-substring start end))
         (result (shell-command-to-string (concat "date -d @" timestamp))))
    (goto-char start)
    (delete-region start end)
    (insert (string-trim-right result))))

