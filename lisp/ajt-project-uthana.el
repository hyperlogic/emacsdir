(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-uthana-path "/home/tony/uthana")

(defun ajt-uthana-code-search (arg)
  "Search for a regex in all uthana python code"
  (interactive "suthana-code:")
  (ajt-grep-find arg (list ajt-uthana-path) '("*.py")))

(defun ajt-uthana-rg (arg)
  "Search for regex in all uthana directory"
  (interactive "suthana-rg:")
  (ajt-ripgrep-find arg ajt-uthana-path (list "!*.ipynb")))

(global-set-key [f7] 'ajt-uthana-code-search)
(global-set-key [f8] 'ajt-uthana-rg)
