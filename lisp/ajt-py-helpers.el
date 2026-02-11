;; The best way to install these tools "globally" is to use `uv tool install black`
;; black
(defun ajt-black ()
  (interactive)
  (save-buffer)
  (let ((exit-code (shell-command (concat "black --line-length=120 " (buffer-file-name)) "*black-log*")))
    (if (zerop exit-code)
        (progn
          (revert-buffer t t t)
          (message "black: success"))
      (pop-to-buffer "*black-log*")
      (compilation-mode))))

;; ty
(defun ajt-ty ()
  (interactive)
  (save-buffer)
  (let ((exit-code (shell-command (concat "ty check " (buffer-file-name)) "*ty-log*")))
    (if (zerop exit-code)
        (message "ty: success")
      (pop-to-buffer "*ty-log*")
      (compilation-mode))))

;; isort
(defun ajt-isort ()
  (interactive)
  (save-buffer)
  (shell-command (concat "isort --profile=black --line-length=120 " (buffer-file-name)) "*flake8-log*")
  (pop-to-buffer "*isort-log*")
  (compilation-mode))


;; M-x treesit-install-language-grammar RET python RET
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")))

(load "project")
(load "eglot")

;; Don't insert goofy shit in my code
(add-hook 'eglot-managed-mode-hook
          (lambda () (eglot-inlay-hints-mode -1)))

(defun my-project-find-python-project (dir)
  (when-let ((root (or (locate-dominating-file dir "pyproject.toml")
                       (locate-dominating-file dir ".git"))))
    (cons 'python-project root)))

(with-eval-after-load "project"
  (cl-defmethod project-root ((project (head python-project)))
    (cdr project))

  (add-hook 'project-find-functions #'my-project-find-python-project))

(add-to-list 'auto-mode-alist '("/uv\\.lock\\'" . toml-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(add-to-list 'eglot-server-programs
             '((python-ts-mode python-mode)
               . ("ty" "server")))
(add-hook 'python-ts-mode-hook #'eglot-ensure)

;; eglot should use ty for lsp
;;(with-eval-after-load 'eglot
;;  (add-to-list 'eglot-server-programs
;;               '(python-mode . ("ty" "server"))))

;;;; help eglot find the root of the project.
;;(defun project-root-override (dir)
;;    "Find project root by looking for .git or pyproject.toml."
;;    (let ((root (or (locate-dominating-file dir ".git")
;;                    (locate-dominating-file dir "pyproject.toml"))))
;;      (and root (cons 'transient root))))

;;(add-hook 'project-find-functions #'project-root-override)
