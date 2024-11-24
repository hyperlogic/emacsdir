(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-uthana-path "~/code/phasefuncnn")

(defun ajt-pfnn-py-search (arg)
  "Search for a regex in all pfnn python code"
  (interactive "pfnn-py:")
  (ajt-ripgrep-find arg ajt-pfnn-path '("*.py")))

(defun ajt-pfnn-all-search (arg)
  "Search for regex in all pfnn directory"
  (interactive "spfnn-all:")
  (ajt-ripgrep-find arg ajt-pfnn-path (list "!*.ipynb")))

(global-set-key [f7] 'ajt-pfnn-py-search)
(global-set-key [f9] 'ajt-pfnn-all-search)

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

