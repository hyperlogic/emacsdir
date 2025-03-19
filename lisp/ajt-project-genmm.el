(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-genmm-path "/home/hyperlogic/code/phasefuncnn")
(setq ajt-venv-exclude-path "!/home/hyperlogic/code/phasefuncnn/venv/*")

(defun ajt-genmm-py-search (arg)
  "Search for a regex in all genmm python code"
  (interactive "sgenmm-py:")
  (ajt-ripgrep-find arg ajt-genmm-path '("*.py")))

(defun ajt-genmm-all-search (arg)
  "Search for regex in all genmm directory"
  (interactive "sgenmm-all:")
  (ajt-ripgrep-find arg ajt-genmm-path (list "!*.ipynb")))

(defun ajt-genmm-run ()
  (interactive)
  (shell-command (concat "cd " ajt-genmm-path "; source venv/Scripts/activate; python char_vis.py") "*genmm-log*")
  (pop-to-buffer "*genmm-log*")
  (compilation-mode))

(global-set-key [f7] 'ajt-genmm-py-search)
(global-set-key [f8] 'ajt-genmm-all-search)
(global-set-key [f10] 'flymake-show-buffer-diagnostics)

;; first install pyright - microsofts pyright server for python
;; pip install pyright
(use-package company)
(use-package lsp-pyright
    :ensure t
    :custom (lsp-pyright-langserver-command "pyright")
    :hook (python-mode . (lambda ()
                           (require 'lsp-pyright)
                           (lsp)
                           (company-mode))))

