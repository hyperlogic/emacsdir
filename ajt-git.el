
;; invoke git blame on current buffer.
(defun ajt-git-blame ()
  (interactive)
  (let ((line (line-number-at-pos)))
    (shell-command (concat "git blame -w \"" (buffer-file-name) "\"") "*ajt-blame*")
    (pop-to-buffer "*ajt-blame*")
    (goto-line line)))

;; invoke git log on current buffer.
(defun ajt-git-log ()
  (interactive)
  (let ((line (line-number-at-pos)))
    (shell-command (concat "git log " (buffer-file-name)) "*ajt-log*")
    (pop-to-buffer "*ajt-log*")
    (goto-line line)))

;; invoke git diff and pipe result into a buffer.
(defun ajt-git-diff ()
  (interactive)
  (shell-command "git diff -w" "*ajt-git-diff*")
  (pop-to-buffer "*ajt-git-diff*")
  (diff-mode))

