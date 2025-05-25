(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-build-config "Debug")
(setq ajt-build-dir "/home/hyperlogic/code/cylindrix/build")
(setq ajt-code-path "/home/hyperlogic/code/cylindrix/src")

(defun ajt-cylindrix-code-search (arg)
  "Search for a regex in all cylindrix code"
  (interactive "scylindrix-code:")
  (ajt-grep-find arg (list ajt-code-path) '("*.cc" "*.cpp" "*.c"  "*.cu" "*.h" "*.hpp" "*.cuh" "*.txt")))

(defun ajt-cylindrix-rg (arg)
  "Search for regex in all cylindrix code"
  (interactive "scylindrix-rg:")
  (ajt-ripgrep-find arg ajt-code-path (list "!*.ipynb")))

(setq-default tab-width 4)

(global-set-key [f7] 'ajt-cylindrix-rg)
(global-set-key [f8] 'ajt-cylindrix-rg)

(setq compile-command (concat "cd " ajt-build-dir "; cmake --build . --config " ajt-build-config))

