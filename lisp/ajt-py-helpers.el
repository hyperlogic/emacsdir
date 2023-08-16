;; black
(defun ajt-black ()
  (interactive)
  (shell-command (concat "black " (buffer-file-name)) "*black-log*")
  (pop-to-buffer "*black-log*")
  (compilation-mode))

;; flake8
(defun ajt-flake8 ()
  (interactive)
  (shell-command (concat "flake8 " (buffer-file-name)) "*flake8-log*")
  (pop-to-buffer "*flake8-log*")
  (compilation-mode))

;; flake8
(defun ajt-mypy ()
  (interactive)
  (shell-command (concat "mypy " (buffer-file-name)) "*mypy-log*")
  (pop-to-buffer "*mypy-log*")
  (compilation-mode))
