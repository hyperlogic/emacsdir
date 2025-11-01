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


;; Install ruff with `pip install ruff`
(use-package lsp-mode)
(use-package flycheck)
(setq lsp-diagnostics-provider :flycheck)
(setq flycheck-indication-mode 'left-margin)

(add-hook 'flycheck-mode-hook
          (lambda ()
            (when (not (display-graphic-p))
              (setq left-margin-width 1)
              (set-window-buffer nil (current-buffer)))))

;; Configure Ruff
(add-to-list 'lsp-language-id-configuration '(python-mode . "python"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection '("ruff" "server"))
  :activation-fn (lsp-activate-on "python")
  :server-id 'ruff))

;; Auto-start lsp for Python files
(add-hook 'python-mode-hook #'lsp)

;; C-c ! n / M-g n - next-error
;; C-c ! p / M-g p - previous-error
;; C-c ! l - flycheck-list-errors
