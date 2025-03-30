(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-camdm-path "~/code/CAMDM-inference")

(defun ajt-camdm-src-search (arg)
  "Search for a regex in all camdm source code"
  (interactive "scamdm-js:")
  (ajt-ripgrep-find arg ajt-camdm-path '("*.js" "*.ts")))

(setq compile-command (concat "cd " ajt-camdm-path "; npm run tsc"))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . electric-pair-mode))

(global-set-key [f7] 'ajt-camdm-src-search)
(global-set-key [f8] 'compile)
