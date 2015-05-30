;; list of header and soruce file extentions
(setq ajt-hdr-ext-list `(".h" ".hpp" ".vsh" ".hh"))
(setq ajt-src-ext-list `(".cpp" ".c" ".m" ".mm" ".fsh" ".cc"))

;; fn can return non-nil to stop iteration
(defun ajt-for-each (fn lst)
  (if (null lst)
      nil
    (if (funcall fn (car lst))
        't
      (ajt-for-each fn (cdr lst)))))

;; if file exists open it and return 't, otherwise return nil
(defun ajt-find-file-with-ext (basename ext)
  (let ((full-filename (concat basename ext)))
    (if (file-exists-p full-filename)
        (progn (find-file full-filename) 't)
      nil)))

;; Swap between .h/.hpp and .cpp/.c/.m/.mm files
(defun ajt-header-swap ()
  "Swap between .h & .cpp files"
  (interactive)
  (let* ((filename (buffer-file-name (current-buffer)))
         (ext (file-name-extension filename))
         (basename (file-name-sans-extension filename)))

    ;; if current-buffer is a header
    (if (member (concat "." ext) ajt-hdr-ext-list)
        ;; try to open corresponding src file
        (if (ajt-for-each (lambda (ext) (ajt-find-file-with-ext basename ext)) ajt-src-ext-list)
            't
          (error "could not find corresponding src file for \"%s\"" filename))
      ;; try to open corresponding header file
      (if (ajt-for-each (lambda (ext) (ajt-find-file-with-ext basename ext)) ajt-hdr-ext-list)
          't
        (error "could not find header file for \"%s\"" filename)))))

;; was mark-whole-buffer
(global-set-key "\C-x\h" 'ajt-header-swap)
(global-set-key "\C-x\C-h" 'ajt-header-swap)
