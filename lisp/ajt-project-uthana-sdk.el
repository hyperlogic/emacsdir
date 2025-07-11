(load "ajt-vector-math")
(load "ajt-grep-find")

(setq ajt-sdk-path "~/code/uthana-sdk")

(setq compile-command (concat "cd " ajt-sdk-path "/core && ./lint.sh && cd ../web && NO_COLOR=true npm run build && cd samples/bones && npm run build_dev"))

(defun ajt-uthana-sdk-cpp-search (arg)
  "Search for a regex in all core cpp code"
  (interactive "suthana-sdk-cpp:")
  (ajt-ripgrep-find arg (concat ajt-sdk-path "/core") '("*.h" "*.c" "*.cc" "*.cpp")))

(defun ajt-uthana-sdk-ts-search (arg)
  "Search for a regex in all core cpp code"
  (interactive "suthana-sdk-ts:")
  (ajt-ripgrep-find arg (concat ajt-sdk-path "/web/src") '("*.ts")))

(use-package typescript-mode :mode "\\.ts\\'")

(use-package cmake-mode)

(global-set-key [f7] 'ajt-uthana-sdk-cpp-search)
(global-set-key [f8] 'ajt-uthana-sdk-ts-search)
(global-set-key [f9] 'compile)

(defun uthana-dev-start (sample-name)
  "Start webpack serve and puppeteer-logger, with output in a tailable buffer."
  (interactive "ssample-name:")
  (let* ((default-directory (concat ajt-sdk-path (concat "/web/samples/" sample-name)))
         (webpack-process-name "uthana-webpack-serve")
         (puppeteer-process-name "uthana-puppeteer-logger")
         (output-buffer-name "*Uthana Dev Output*")
         (output-buffer (get-buffer-create output-buffer-name)))

    ;; Kill any existing processes
    (when (get-process webpack-process-name)
      (delete-process (get-process webpack-process-name)))
    (when (get-process puppeteer-process-name)
      (delete-process (get-process puppeteer-process-name)))

    ;; Start webpack serve in background
    (start-process-shell-command webpack-process-name nil "npx webpack serve")

    ;; Start puppeteer-logger and capture output
    (let ((puppeteer-process (start-process puppeteer-process-name
                                           output-buffer
                                           "npx" "node" "../../puppeteer-logger.js")))

      ;; Set up the output buffer
      (with-current-buffer output-buffer
        (erase-buffer)
        (special-mode)
        (setq-local mode-line-process '(":%s"))
        (setq-local buffer-read-only t)

        ;; Store process references
        (setq-local uthana-webpack-process (get-process webpack-process-name))
        (setq-local uthana-puppeteer-process puppeteer-process)

        ;; Add cleanup hook when buffer is killed
        (add-hook 'kill-buffer-hook
                  (lambda ()
                    (when (and (boundp 'uthana-webpack-process)
                               uthana-webpack-process
                               (process-live-p uthana-webpack-process))
                      (delete-process uthana-webpack-process))
                    (when (and (boundp 'uthana-puppeteer-process)
                               uthana-puppeteer-process
                               (process-live-p uthana-puppeteer-process))
                      (delete-process uthana-puppeteer-process)))
                  nil t))

      ;; Display the buffer
      (pop-to-buffer output-buffer)

	  ;; Enable auto-revert (tail -f behavior)
      (auto-revert-tail-mode 1)
      (setq-local auto-revert-interval 0.5)

      (message "Started webpack serve and puppeteer-logger. Close %s to stop both processes."
               output-buffer-name))))
