(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-uthana-path "/home/tony/uthana")
(setq ajt-uthana-log "/opt/uthana/log/appserv.log")

(defun ajt-uthana-py-search (arg)
  "Search for a regex in all uthana python code"
  (interactive "suthana-py:")
  (ajt-ripgrep-find arg ajt-uthana-path '("*.py")))

(defun ajt-uthana-js-search (arg)
  "Search for a regex in all uthana javascript code"
  (interactive "suthana-js:")
  (ajt-ripgrep-find arg ajt-uthana-path '("*.js")))

(defun ajt-uthana-all-search (arg)
  "Search for regex in all uthana directory"
  (interactive "suthana-all:")
  (ajt-ripgrep-find arg ajt-uthana-path (list "!*.ipynb")))

; tail -f /opt/uthana/log/appserv.log
(defun ajt-uthana-log ()
  (interactive)
  (find-file ajt-uthana-log)
  (auto-revert-tail-mode)
  (buffer-disable-undo))

(global-set-key [f7] 'ajt-uthana-py-search)
(global-set-key [f8] 'ajt-uthana-js-search)
(global-set-key [f9] 'ajt-uthana-all-search)

(server-start)
