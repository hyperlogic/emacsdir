(setq ajt-hifi-path "~/code/hifi")
(if is-windows-machine
      (setq ajt-hifi-path "C:/Users/Anthony/code/hifi"))

;; hifi javascript search with regex
(defun ajt-hifi-js-search (arg)
  "Search for a regex in all hifi javascript files"
  (interactive "shifi-js:")
  (let ((path (concat ajt-hifi-path "/examples")))
    (ajt-grep-find arg (list path) '("*.js"))))

;; WebGame cpp search with regex
(defun ajt-hifi-cpp-search (arg)
  "Search for a regex in all hifi cpp files"
  (interactive "shifi-cpp:")
  (ajt-grep-find arg (list ajt-hifi-path) '("*.cc" "*.cpp" "*.h" "*.hpp")))

(global-set-key [f8] 'ajt-hifi-js-search)
(global-set-key [f9] 'ajt-hifi-cpp-search)
