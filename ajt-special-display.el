;;
;; I frequently use compile, grep and shell windows.
;; However during long sessions I find it annoying to have to shuffle and kill these windows all the time.
;; In an attempt to prevent this I tag these windows as "special".
;;
;; When "special" buffers are displayed they should appear in the lower right side of the frame.
;; Unless, there is already an existing "special" buffer being displayed, if so then use that window instead.
;;

;; all windows should be pop-up.
;; NOTE: specifically this allows the "*shell*" buffer to go through the normal display-buffer logic.
;; which allows me to treat it specially below.
(setq same-window-buffer-names nil)

;; list of "special" buffers, add new ones here.
(setq ajt-special-buffers `("*compilation*" "*grep*" "*shell*" "*ajt-grep*" "*ansi-term*" "*adb-logcat*" "*ajt-blame*" "*ajt-log*" "*ajt-ngcore*"))

;; Customize special-display-buffer-names, this will cause the ajt-special-display function to be called on these buffers
;; instead of the standard display-buffer
(setq ajt-special-display-buffer-names (mapcar (lambda (x) (cons x '(ajt-special-display))) ajt-special-buffers))
(setq special-display-buffer-names ajt-special-display-buffer-names)

(defun ajt-lr-cmp (a b)
  "Returns the lower-right most of the two windows a and b."
  (if (eq b nil)
      a
    (let* ((ae (window-edges a)) (be (window-edges b))
           (ax (nth 2 ae)) (ay (nth 3 ae))
           (bx (nth 2 be)) (by (nth 3 be)))
      (cond ((> ay by) a)
            ((< ay by) b)
            ((> ax bx) a)
            ((< ax bx) b)
            (t b)))))

(defun ajt-lr-window (l)
  "Returns the lower-right most window in the list"
  (if (eq l nil)
      nil
    (ajt-lr-cmp (car l) (ajt-lr-window (cdr l)))))

(defun ajt-split-window-thirds()
 "Split window into thirds"
 (interactive)
 (if (= 1 (length (window-list)))
     (let* ((width (third (window-edges (car (window-list))))))
       (split-window-horizontally (/ width -3))
       (split-window-horizontally (/ width -3)))))

(setq ajt-split-height-threshold 75)

(defun ajt-split-window-preferred-function (x)
  "Always split the lower right most window, unless there is only one window."
  "In that case split it horizontaly"
  (if (= (length (window-list)) 1)
      (split-window-horizontally)
    (let* ((lr-window (ajt-lr-window (window-list)))
           (edges (window-edges lr-window))
           (height (- (nth 3 edges) (nth 1 edges))))
      (if (> height ajt-split-height-threshold)
          (split-window lr-window)
        lr-window))))

(defun ajt-special-display (buffer-or-name)
  "If there is window with a special buffer already open, display the buffer in that window"
  "Otherwise vertical split the lower right most window and display the buffer in the new bottom pane"
  (let ((existing (get-window-with-predicate (lambda (w) (member (buffer-name (window-buffer w)) ajt-special-buffers)))))
    (if existing
        (progn
          (set-window-buffer existing buffer-or-name)
          existing)
      (progn
        ;; reuse the existing display-buffer logic, except
        ;; remove the special-display-buffer-names so we don't get into an infinite recursion.
        (setq special-display-buffer-names nil)

        ;; install a custom window splitter function which splits the lower-right window
        (setq split-window-preferred-function #'ajt-split-window-preferred-function)

        ;; call display-buffer and store the result
        (let ((result (display-buffer buffer-or-name)))

          ;; restore the original values
          (setq split-window-preferred-function #'split-window-sensibly)
          (setq special-display-buffer-names ajt-special-display-buffer-names)
          result)))))
