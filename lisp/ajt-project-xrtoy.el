(load "ajt-vector-math")

(setq ajt-xrtoy-project-path "C:/msys64/home/ajthy/code/xrtoy")

;; xrtoy cpp search with regex
(defun ajt-xrtoy-project-cpp-search (arg)
  "Search for a regex in all xrtoy project cpp files"
  (interactive "sxrtoy-cpp:")
  (ajt-grep-find arg (list (concat ajt-xrtoy-project-path "/src")) '("*.h" "*.cpp")))

;; xrtoy lua search with regex
(defun ajt-xrtoy-project-lua-search (arg)
  "Search for a regex in all xrtoy lua files"
  (interactive "sxrtoy-lua:")
  (ajt-grep-find arg (list (concat ajt-xrtoy-project-path "/lua")) '("*.lua")))

(global-set-key [f9] 'ajt-xrtoy-project-cpp-search)
(global-set-key [f10] 'ajt-xrtoy-project-lua-search)


