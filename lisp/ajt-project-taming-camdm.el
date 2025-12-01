(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-project-path (concat "/home/" username "/code/taming-camdm"))
(setq ajt-project-exclude-path (concat "!/home/" username "/code/taming-camdm/.venv/*"))

(setq compile-command (concat "cd " ajt-project-path " && uv run snakemake --cores 32 cook"))

(defun ajt-project-py-search (arg)
  "Search for a regex in project python code"
  (interactive "sproject-py:")
  (ajt-ripgrep-find arg ajt-project-path '("*.py")))

(defun ajt-project-all-search (arg)
  "Search for regex in project directory"
  (interactive "sproject-all:")
  (ajt-ripgrep-find arg ajt-project-path (list "!*.ipynb")))

(global-set-key [f7] 'ajt-project-py-search)
(global-set-key [f8] 'ajt-project-all-search)
(global-set-key [f9] 'compile)
