(load "ajt-vector-math")
(load "ajt-grep-find")

(if (string-equal hostname "tony.uthana.dev")
    (setq ajt-uthana-path "/home/tony/uthana")
  (setq ajt-uthana-path "~/uthana/uthana"))
(setq ajt-uthana-log "/opt/uthana/log/appserv.log")

(setq ajt-uthana-run-path (concat ajt-uthana-path "/poselib/tools/"))

(defun ajt-uthana-py-search (arg)
  "Search for a regex in all uthana python code"
  (interactive "suthana-py:")
  (ajt-ripgrep-find arg ajt-uthana-path '("*.py")))

(defun ajt-uthana-ts-search (arg)
  "Search for a regex in all uthana javascript code"
  (interactive "suthana-ts:")
  (ajt-ripgrep-find arg ajt-uthana-path '("*.ts" "*.tsx")))

(defun ajt-uthana-all-search (arg)
  "Search for regex in all uthana directory"
  (interactive "suthana-all:")
  (ajt-ripgrep-find arg ajt-uthana-path (list "!*.ipynb")))

(use-package log4j-mode)

; tail -f /opt/uthana/log/appserv.log
(defun ajt-uthana-log ()
  (interactive)
  (find-file ajt-uthana-log)
  (auto-revert-tail-mode)
  (log4j-mode)
  (buffer-disable-undo))

;; nsq_tail -lookupd-http-address=127.0.0.1:4161 -topic=event | jq .
;; TODO: DOES NOT WORK
(defun ajt-uthana-analytics ()
  (interactive)
  (shell-command (concat "cd " ajt-uthana-run-path "; nsq_tail -lookupd-http-address=127.0.0.1:4161 -topic=event | jq .") "*uthana-analytics*")
  (pop-to-buffer "*uthana-run-log*")
  (auto-revert-tail-mode)
  ;(log4j-mode)
  (buffer-disable-undo))

(defun ajt-run-uthana ()
  (interactive)
  (shell-command (concat "cd " ajt-uthana-run-path "; python3 ik_test.py") "*uthana-run-log*")
  (pop-to-buffer "*uthana-run-log*")
  (compilation-mode))

;;(global-set-key [f7] 'ajt-run-uthana)
(global-set-key [f7] 'ajt-uthana-py-search)
(global-set-key [f8] 'ajt-uthana-ts-search)
(global-set-key [f9] 'ajt-uthana-all-search)

