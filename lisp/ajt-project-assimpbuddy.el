(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-assimpbuddy-path "~/code/assimpbuddy")

(setq compile-command (concat "cd " ajt-assimpbuddy-path " && ./lint.sh && cd build && NO_COLOR=true cmake --build . --config Debug"))

(use-package google-c-style)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(defun ajt-assimpbuddy-cpp-search (arg)
  "Search for a regex in all core cpp code"
  (interactive "sassimpbuddy-cpp:")
  (ajt-ripgrep-find arg (concat ajt-assimpbuddy-path "/src") '("*.h" "*.c" "*.cc" "*.cpp")))

(global-set-key [f7] 'ajt-assimpbuddy-cpp-search)
(global-set-key [f9] 'compile)
