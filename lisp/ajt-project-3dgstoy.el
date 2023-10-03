(load "ajt-vector-math")

(setq compile-command (concat "cd ~/code2/3dgstoy/build; cmake --build . --config Debug"))

(defun ajt-run-3dgstoy ()
  (interactive)
  (shell-command "cd ~/code2/3dgstoy/build/Debug; 3dgstoy.exe" "*3dgstoy-log*")
  (pop-to-buffer "*3dgstoy-log*")
  (compilation-mode))

(global-set-key [f7] 'recompile)
(global-set-key [f8] 'ajt-run-3dgstoy)

