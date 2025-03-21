;; black
(defun ajt-black ()
  (interactive)
  (save-buffer)
  (shell-command (concat "black --line-length=120 " (buffer-file-name)) "*black-log*")
  (pop-to-buffer "*black-log*")
  (compilation-mode))

;; flake8
(defun ajt-flake8 ()
  (interactive)
  (save-buffer)
  (shell-command (concat "flake8 " (buffer-file-name)) "*flake8-log*")
  (pop-to-buffer "*flake8-log*")
  (compilation-mode))

;; isort
(defun ajt-isort ()
  (interactive)
  (save-buffer)
  (shell-command (concat "isort --profile=black --line-length=120 " (buffer-file-name)) "*flake8-log*")
  (pop-to-buffer "*isort-log*")
  (compilation-mode))

;; isort
(defun ajt-mypy ()
  (interactive)
  (save-buffer)
  (shell-command (concat "mypy " (buffer-file-name)) "*mypy-log*")
  (pop-to-buffer "*mypy-log*")
  (compilation-mode))

