(load "ajt-vector-math")

(setq ajt-3dgs-config "Debug")
(setq ajt-3dgs-build-dir "~/code/3dgstoy/build")

(setq compile-command (concat "cd " ajt-3dgs-build-dir "; cmake --build . --config " ajt-3dgs-config))

(defun ajt-run-3dgstoy ()
  (interactive)
  (shell-command (concat "cd " ajt-3dgs-build-dir "/" ajt-3dgs-config "; ./3dgstoy.exe") "*3dgstoy-log*")
  (pop-to-buffer "*3dgstoy-log*")
  (compilation-mode))

(global-set-key [f7] 'recompile)
(global-set-key [f8] 'ajt-run-3dgstoy)

