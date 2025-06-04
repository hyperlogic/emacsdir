;; eslint
(defun ajt-eslint ()
  (interactive)
  (shell-command (concat "eslint " (buffer-file-name) " --no-eslintrc --rule 'no-unused-vars:error' --rule 'semi:error' --rule 'max-len:[\"error\", 120]'") "*eslint-log*")
  (pop-to-buffer "*eslint-log*")
  (compilation-mode))

;; jsonlint
(defun ajt-jsonlint ()
  (interactive)
  (shell-command (concat "jq . \"" (buffer-file-name) "\"") "*jsonlint-log*")
  (pop-to-buffer "*jsonlint-log*")
  (compilation-mode))


