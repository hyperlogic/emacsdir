;; eslint
(defun ajt-eslint ()
  (interactive)
  (shell-command (concat "eslint -f unix " (buffer-file-name)) "*eslint-log*")
  (pop-to-buffer "*eslint-log*")
  (compilation-mode))

;; jsonlint
(defun ajt-jsonlint ()
  (interactive)
  (shell-command (concat "jsonlint \"" (buffer-file-name) "\"") "*jsonlint-log*")
  (pop-to-buffer "*jsonlint-log*")
  (compilation-mode))
