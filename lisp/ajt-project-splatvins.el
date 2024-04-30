(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-splatvins-path "/home/hyperlogic/ros/catkin_ws_ov/src/Splat_VINS/")

(defun ajt-splatvins-code-search (arg)
  "Search for a regex in all splatvins code"
  (interactive "ssplatvins-code:")
  (ajt-grep-find arg (list ajt-splatvins-path) '("*.cc" "*.cpp" "*.c"  "*.cu" "*.h" "*.hpp" "*.cuh" "*.txt")))

(defun ajt-splatvins-rg (arg)
  "Search for regex in all splatvins code"
  (interactive "ssplatvins-rg:")
  (ajt-ripgrep-find arg ajt-splatvins-path (list "!*.ipynb")))

(setq-default tab-width 4)

(global-set-key [f7] 'ajt-splatvins-rg)
(global-set-key [f8] 'ajt-splatvins-rg)

