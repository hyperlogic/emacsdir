;; Enable OSC 52 clipboard integration
(unless (display-graphic-p)
  (require 'xclip nil t)

  ;; If xclip package is not available, use this manual implementation
  (when (not (featurep 'xclip))
    (defun my-osc-52-copy (text)
      "Copy TEXT to clipboard using OSC 52 escape sequence."
      (let* ((encoded (base64-encode-string (encode-coding-string text 'utf-8) t))
             (osc-seq (concat "\e]52;c;" encoded "\a")))
        (send-string-to-terminal osc-seq)))

    (setq interprogram-cut-function 'my-osc-52-copy)))
