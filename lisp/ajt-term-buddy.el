;; ajt-term-buddy.el
;;
;; Keeps a *vterm* buffer always pinned to the bottom right quarter of the screen.
;; Any new buffers that pop up should occupy the left hand pane, or the right hand pane above the *vterm*
;; but the *vterm* is always there.

(require 'vterm nil t)

(defvar ajt-term-buddy-buffer-name "*vterm*"
  "Name of the vterm buffer to keep pinned.")

(defvar ajt-term-buddy-enabled nil
  "Non-nil when term-buddy mode is active.")

(defvar ajt-term-buddy-upper-right-window nil
  "The upper-right window for popup buffers.")

(defun ajt-term-buddy-get-vterm-window ()
  "Return the window displaying the vterm buffer, or nil if not visible."
  (get-buffer-window ajt-term-buddy-buffer-name))

(defun ajt-term-buddy-vterm-exists-p ()
  "Return non-nil if the vterm buffer exists."
  (get-buffer ajt-term-buddy-buffer-name))

(defun ajt-term-buddy-setup-layout ()
  "Set up the term-buddy window layout with vterm in bottom-right."
  (interactive)
  (delete-other-windows)
  ;; Save reference to left window before splitting
  (let ((left-window (selected-window)))
    ;; Split horizontally first (left | right)
    (split-window-horizontally)
    ;; Move to right window
    (other-window 1)
    ;; Split right window vertically (top-right / bottom-right)
    (split-window-vertically)
    ;; Save reference to upper-right window (current window after split)
    (setq ajt-term-buddy-upper-right-window (selected-window))
    ;; Move to bottom-right window
    (other-window 1)
    ;; Create or switch to vterm in bottom-right
    (if (ajt-term-buddy-vterm-exists-p)
        (switch-to-buffer ajt-term-buddy-buffer-name)
      (vterm))
    ;; Move back to left window for editing
    (select-window left-window))
  (setq ajt-term-buddy-enabled t)
  (message "Term-buddy layout activated"))

(defun ajt-term-buddy-display-buffer-function (buffer alist)
  "Custom display function that respects the term-buddy layout.
Displays BUFFER in the upper-right pane only, leaving left pane unchanged."
  (when (and ajt-term-buddy-enabled
             (window-live-p ajt-term-buddy-upper-right-window))
    (set-window-buffer ajt-term-buddy-upper-right-window buffer)
    ajt-term-buddy-upper-right-window))

(defun ajt-term-buddy-protect-vterm (orig-fun &rest args)
  "Advice to prevent vterm window from being deleted or replaced."
  (let ((vterm-window (ajt-term-buddy-get-vterm-window)))
    (if (and ajt-term-buddy-enabled
             vterm-window
             (eq (selected-window) vterm-window))
        ;; If we're in vterm window and it would be affected, select another window first
        (let ((other-win (get-window-with-predicate
                          (lambda (w) (not (eq w vterm-window))))))
          (when other-win
            (select-window other-win))
          (apply orig-fun args))
      (apply orig-fun args))))

(defun ajt-term-buddy-ensure-vterm-visible ()
  "Ensure vterm buffer is visible. Re-display if it was hidden."
  (when ajt-term-buddy-enabled
    ;; Check if upper-right window is still valid
    (unless (window-live-p ajt-term-buddy-upper-right-window)
      (setq ajt-term-buddy-upper-right-window
            (ajt-term-buddy-find-upper-right-window)))
    ;; Ensure vterm is visible
    (when (and (ajt-term-buddy-vterm-exists-p)
               (not (ajt-term-buddy-get-vterm-window)))
      ;; vterm exists but is not visible, we need to show it
      (let ((bottom-right (ajt-term-buddy-find-bottom-right-window)))
        (when bottom-right
          (with-selected-window bottom-right
            (split-window-vertically)
            (other-window 1)
            (switch-to-buffer ajt-term-buddy-buffer-name)))))))

(defun ajt-term-buddy-find-bottom-right-window ()
  "Find the bottom-right most window (excluding vterm if visible)."
  (let ((best nil)
        (best-score -1)
        (vterm-window (ajt-term-buddy-get-vterm-window)))
    (dolist (win (window-list))
      (unless (eq win vterm-window)
        (let* ((edges (window-edges win))
               (right (nth 2 edges))
               (bottom (nth 3 edges))
               (score (+ (* bottom 10000) right)))
          (when (> score best-score)
            (setq best win)
            (setq best-score score)))))
    best))

(defun ajt-term-buddy-find-upper-right-window ()
  "Find the upper-right window (high x, low y, excluding vterm)."
  (let ((best nil)
        (best-score -1)
        (vterm-window (ajt-term-buddy-get-vterm-window)))
    (dolist (win (window-list))
      (unless (eq win vterm-window)
        (let* ((edges (window-edges win))
               (left (nth 0 edges))
               (top (nth 1 edges))
               (right (nth 2 edges))
               ;; Score: prefer rightmost (high right), then topmost (low top)
               ;; Invert top so lower values score higher
               (score (+ (* right 10000) (- 10000 top))))
          (when (and (> left 0)  ;; Must be on right side (not leftmost)
                     (> score best-score))
            (setq best win)
            (setq best-score score)))))
    best))

(defun ajt-term-buddy-window-delete-advice (orig-fun &optional window)
  "Prevent deletion of vterm window when term-buddy is active."
  (let* ((win (or window (selected-window)))
         (vterm-window (ajt-term-buddy-get-vterm-window)))
    (if (and ajt-term-buddy-enabled
             vterm-window
             (eq win vterm-window))
        (message "Cannot delete vterm window while term-buddy is active. Use M-x ajt-term-buddy-disable")
      (funcall orig-fun window))))

(defun ajt-term-buddy-server-window (buffer)
  "Display BUFFER in the upper-right window for emacsclient."
  (if (and ajt-term-buddy-enabled
           (window-live-p ajt-term-buddy-upper-right-window))
      (progn
        (select-window ajt-term-buddy-upper-right-window)
        (switch-to-buffer buffer))
    ;; Fallback if upper-right window is not available
    (switch-to-buffer buffer)))

(defun ajt-term-buddy-enable ()
  "Enable term-buddy mode and set up the layout."
  (interactive)
  (ajt-term-buddy-setup-layout)
  ;; Add advice to protect vterm window from deletion
  (advice-add 'delete-window :around #'ajt-term-buddy-window-delete-advice)
  ;; Add hook to ensure vterm stays visible after window configuration changes
  (add-hook 'window-configuration-change-hook #'ajt-term-buddy-ensure-vterm-visible)
  ;; Configure display-buffer to respect our layout (match all buffers)
  (add-to-list 'display-buffer-alist
               '("." (ajt-term-buddy-display-buffer-function)))
  ;; Configure emacsclient to use upper-right window
  (setq server-window #'ajt-term-buddy-server-window)
  (message "Term-buddy enabled"))

(defun ajt-term-buddy-disable ()
  "Disable term-buddy mode."
  (interactive)
  (setq ajt-term-buddy-enabled nil)
  (setq ajt-term-buddy-upper-right-window nil)
  (setq server-window nil)
  (advice-remove 'delete-window #'ajt-term-buddy-window-delete-advice)
  (remove-hook 'window-configuration-change-hook #'ajt-term-buddy-ensure-vterm-visible)
  (setq display-buffer-alist
        (cl-remove-if (lambda (entry)
                        (member 'ajt-term-buddy-display-buffer-function entry))
                      display-buffer-alist))
  (message "Term-buddy disabled"))

(defun ajt-term-buddy-toggle ()
  "Toggle term-buddy mode on or off."
  (interactive)
  (if ajt-term-buddy-enabled
      (ajt-term-buddy-disable)
    (ajt-term-buddy-enable)))

(defun ajt-term-buddy-focus-vterm ()
  "Switch focus to the vterm window."
  (interactive)
  (let ((vterm-window (ajt-term-buddy-get-vterm-window)))
    (if vterm-window
        (select-window vterm-window)
      (message "vterm window not found"))))

(defun ajt-term-buddy-focus-editor ()
  "Switch focus away from vterm to an editor window."
  (interactive)
  (let ((vterm-window (ajt-term-buddy-get-vterm-window)))
    (when vterm-window
      (let ((other-win (get-window-with-predicate
                        (lambda (w) (not (eq w vterm-window))))))
        (when other-win
          (select-window other-win))))))

(defun ajt-term-buddy-enable-deferred ()
  "Enable term-buddy after desktop restore completes.
Use this in your init file instead of `ajt-term-buddy-enable' when using desktop-save-mode."
  (if (and (boundp 'desktop-save-mode) desktop-save-mode)
      (add-hook 'desktop-after-read-hook #'ajt-term-buddy-enable)
    ;; No desktop mode, use window-setup-hook as fallback
    (add-hook 'window-setup-hook #'ajt-term-buddy-enable)))

(provide 'ajt-term-buddy)
