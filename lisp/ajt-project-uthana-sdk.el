(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-sdk-path "~/code/uthana-sdk")

(setq compile-command (concat "cd " ajt-sdk-path "/core && ./lint.sh && cd ../web && NO_COLOR=true npm run build && cd samples/demo && npm run build_dev"))

(defun ajt-run-demo ()
  (interactive)
  (shell-command (concat "cd " ajt-sdk-path "/web/samples/demo && NO_COLOR=true npm start") "*npm-log*")
  (pop-to-buffer "*npm-log*")
  (compilation-mode))


(use-package typescript-mode :mode "\\.ts\\'")

(global-set-key [f8] 'compile)
