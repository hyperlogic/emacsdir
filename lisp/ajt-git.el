
;; invoke git blame on current buffer.
(defun ajt-git-blame ()
  (interactive)
  (let ((line (line-number-at-pos))
        (file-name (file-name-nondirectory (buffer-file-name))))
    (shell-command (concat "git blame -w \"" file-name "\"") "*ajt-blame*")
    (pop-to-buffer "*ajt-blame*")
    (goto-line line)))

;; invoke git log on current buffer.
(defun ajt-git-log ()
  (interactive)
  (let ((line (line-number-at-pos))
        (file-name (file-name-nondirectory (buffer-file-name))))
    (shell-command (concat "git log \"" file-name "\"") "*ajt-git-log*")
    (pop-to-buffer "*ajt-git-log*")))

;; invoke git diff and pipe result into a buffer.
(defun ajt-git-diff ()
  (interactive)
  (shell-command "git diff -w" "*ajt-git-diff*")
  (pop-to-buffer "*ajt-git-diff*")
  (diff-mode))


