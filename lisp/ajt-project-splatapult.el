(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-splatapult-config "Debug")
(setq ajt-splatapult-build-dir "~/code/splatapult/build")
(setq ajt-splatapult-dir "~/code/splatapult")

(setq compile-command (concat "cd " ajt-splatapult-build-dir "; cmake --build . --config " ajt-splatapult-config))

(defun ajt-run-splatapult ()
  (interactive)
  (shell-command (concat "cd " ajt-splatapult-build-dir "/" ajt-splatapult-config "; ./splatapult.exe -d ../../data/bicycle/point_cloud/iteration_30000/point_cloud.ply") "*splatapult-log*")
  (pop-to-buffer "*splatapult-log*")
  (compilation-mode))

(defun ajt-splatapult-rg (arg)
  "Search for regex in all splatvins code"
  (interactive "ssplatapult-rg:")
  (ajt-ripgrep-find arg ajt-splatapult-dir ()))

(global-set-key [f7] 'recompile)
(global-set-key [f8] 'ajt-run-splatapult)
(global-set-key [f9] 'ajt-splatapult-rg)


