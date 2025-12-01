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

;; eglot should use pyright for lsp
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(python-mode . ("pyright-langserver" "--stdio"))))

;; help eglot find the root of the project.
(defun project-root-override (dir)
    "Find project root by looking for .git or pyproject.toml."
    (let ((root (or (locate-dominating-file dir ".git")
                    (locate-dominating-file dir "pyproject.toml"))))
      (and root (cons 'transient root))))

(add-hook 'project-find-functions #'project-root-override)
