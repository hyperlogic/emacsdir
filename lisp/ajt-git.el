
;; invoke git blame on current buffer.
(defun ajt-git-blame ()
  (interactive)
  (let ((line (line-number-at-pos))
        (file-name (file-name-nondirectory (buffer-file-name))))
    (shell-command (concat "git blame -w \"" file-name "\"") "*ajt-blame*")
    (pop-to-buffer "*ajt-blame*")
    (goto-line line)))

;; invoke git log on current buffer.
;; alias gl="git --no-pager log -15 --graph --pretty=\"tformat:%C(green)%h %C(red)%an %C(white)%s %C(green)%cr%C(reset)\" --abbrev-commit"
(defun ajt-git-log ()
  (interactive)
  (let ((line (line-number-at-pos))
        (file-name (file-name-nondirectory (buffer-file-name))))
    (shell-command (concat "git --no-pager log -100 --graph --pretty=\"tformat:%h %an %s %cr\" --abbrev-commit \"" file-name "\"") "*ajt-git-log*")
    (pop-to-buffer "*ajt-git-log*")))


;; invoke git log on current buffer.
(defun ajt-git-log-full ()
  (interactive)
  (let ((line (line-number-at-pos))
        (file-name (file-name-nondirectory (buffer-file-name))))
    (shell-command (concat "git log -p \"" file-name "\"") "*ajt-git-log*")
    (pop-to-buffer "*ajt-git-log*")))

;; invoke git diff and pipe result into a buffer.
(defun ajt-git-diff ()
  (interactive)
  (shell-command "git diff -w" "*ajt-git-diff*")
  (pop-to-buffer "*ajt-git-diff*")
  (diff-mode))


