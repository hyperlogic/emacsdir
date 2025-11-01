(load "ajt-vector-math")
(load "ajt-grep-find")


(setq ajt-pfnn-path (concat "/home/" username "/code/phasefuncnn"))
(setq ajt-venv-exclude-path (concat "!/home/" username "/code/phasefuncnn/venv/*"))

(defun ajt-pfnn-py-search (arg)
  "Search for a regex in all pfnn python code"
  (interactive "spfnn-py:")
  (ajt-ripgrep-find arg ajt-pfnn-path '("*.py")))

(defun ajt-pfnn-all-search (arg)
  "Search for regex in all pfnn directory"
  (interactive "spfnn-all:")
  (ajt-ripgrep-find arg ajt-pfnn-path (list "!*.ipynb")))

(defun ajt-pfnn-run ()
  (interactive)
  (shell-command (concat "cd " ajt-pfnn-path "; source venv/Scripts/activate; python char_vis.py") "*pfnn-log*")
  (pop-to-buffer "*pfnn-log*")
  (compilation-mode))

(global-set-key [f7] 'ajt-pfnn-py-search)
(global-set-key [f8] 'ajt-pfnn-all-search)
