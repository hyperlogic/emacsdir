(load "ajt-vector-math")

(setq ajt-splatapult-config "Debug")
(setq ajt-splatapult-build-dir "~/code/splatapult/build")

(setq compile-command (concat "cd " ajt-splatapult-build-dir "; cmake --build . --config " ajt-splatapult-config))

(defun ajt-run-splatapult ()
  (interactive)
  (shell-command (concat "cd " ajt-splatapult-build-dir "/" ajt-splatapult-config "; ./splatapult.exe -d ../../data/bicycle/point_cloud/iteration_30000/point_cloud.ply") "*splatapult-log*")
  (pop-to-buffer "*splatapult-log*")
  (compilation-mode))

(global-set-key [f7] 'recompile)
(global-set-key [f8] 'ajt-run-splatapult)

