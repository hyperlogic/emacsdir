(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-runningman-path "C:/msys64/home/hyperlogic/code/runningman")
(setq ajt-runningman-config "Debug")

(setq compile-command (concat "cd " ajt-runningman-path "/build/" ajt-runningman-config "; cmake --build .. --config " ajt-runningman-config))

(defun ajt-runningman-cpp-search (arg)
  "Search for a regex in all cpp code"
  (interactive "srunningman-cpp:")
  (ajt-ripgrep-find arg ajt-runningman-path '("*.h" "*.cpp")))

(defun ajt-runningman-all-search (arg)
  "Search for regex in dir"
  (interactive "srunningman-all:")
  (ajt-ripgrep-find arg ajt-runningman-path (list "!*.ipynb")))

(defun ajt-run-runningman ()
  (interactive)
  (shell-command (concat "cd " ajt-runningman-path "/build/" ajt-runningman-config "; ./runningman.exe -d") "*runningman-log*")
  (pop-to-buffer "*runningman-log*")
  (compilation-mode))

(global-set-key [f7] 'recompile)
(global-set-key [f8] 'ajt-run-runningman)
(global-set-key [f9] 'ajt-runningman-cpp-search)
(global-set-key [f10] 'ajt-runningman-all-search)
