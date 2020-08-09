;; list of header and soruce file extentions
(setq ajt-hdr-path-list `("./" "../Public/" "../Classes/Engine/"))
(setq ajt-hdr-ext-list `("h" "hpp" "vsh" "hh" "slv"))
(setq ajt-src-path-list `("./" "../Private/" "../../Private/"))
(setq ajt-src-ext-list `("cpp" "c" "m" "mm" "fsh" "cc" "slf"))

;; fn can return non-nil to stop iteration
(defun ajt-for-each (fn lst)
  (if (null lst)
      nil
    (if (funcall fn (car lst))
        't
      (ajt-for-each fn (cdr lst)))))

;; flatten a list of lists into a single list
;; for example (ajt-flatten '((a b) (c d))) => (a b c d)
(defun ajt-flatten (list-of-lists)
  (apply #'append list-of-lists))

;; build list of expanded filenames to search for.
(defun ajt-build-filenames ()
  (let* ((filename (buffer-file-name (current-buffer)))
         (ext (file-name-extension filename))
         (dir (file-name-directory filename))
         (base (file-name-base filename)))
    (if (member ext ajt-hdr-ext-list)
        (ajt-flatten (mapcar (lambda (p) (mapcar (lambda (e) (concat dir p base "." e)) ajt-src-ext-list)) ajt-src-path-list))
      (ajt-flatten (mapcar (lambda (p) (mapcar (lambda (e) (concat dir p base "." e)) ajt-hdr-ext-list)) ajt-hdr-path-list)))))

;; Swap between .h/.hpp and .cpp/.c/.m/.mm files
(defun ajt-header-swap ()
  "Swap between .h & .cpp files"
  (interactive)
  (let ((filenames (ajt-build-filenames)))
    (if (ajt-for-each (lambda (f) (if (file-exists-p f) (progn (find-file f) 't) nil)) filenames)
        't
      (error "could not find corresponding file for \"%s\"" (buffer-filename (current-buffer))))))

;; was mark-whole-buffer
(global-set-key "\C-x\h" 'ajt-header-swap)
(global-set-key "\C-x\C-h" 'ajt-header-swap)
