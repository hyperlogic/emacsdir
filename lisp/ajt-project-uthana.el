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
(global-set-key [f10] 'flymake-show-diagnostics-buffer)

;; TODO: try lsp-bridge for faster lsp support, or eglot?

;; first install pyright - microsofts pyright server for python
;; pip install pyright
(if (string-equal hostname "tony.uthana.dev")
    (progn
      (server-start)
      (desktop-save-mode 1)
      (use-package company)))
      (use-package lsp-pyright
        :ensure t
        :custom (lsp-pyright-langserver-command "pyright")
        :hook (python-mode . (lambda ()
                               (require 'lsp-pyright)
                               (lsp)
                               (company-mode))))
      (use-package lsp-ui
        :ensure t
        :hook (lsp-mode . lsp-ui-mode)
        :config
        (setq lsp-ui-sideline-enable t
              lsp-ui-sideline-show-diagnostics t
              lsp-ui-doc-enable t                  ;; ← enable doc popup
              lsp-ui-doc-position 'at-point       ;; or 'top'/'bottom'/'left'/'right'
              lsp-ui-doc-delay 0.2
              lsp-ui-doc-show-with-cursor t       ;; popup when cursor is on symbol
              lsp-ui-doc-show-with-mouse nil))    ;; disable mouse hover if not wanted


;; useful commands
;; -----------------------------
;; lsp-find-references - list references to thing at point, 'M-?'
;; flymake-show-diagnostics-buffer - show all warnings errors in file
;; lsp-find-definitions - bound to 'M-.'
;;
;; TODO: try using dap-mode, dap-python and debugpy for integrated debugging

