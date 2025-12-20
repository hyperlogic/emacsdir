;;
;; init.el -  This is my emacs init file. There are many like it, but this one is mine.
;;

;; used to determine which system are we running on.
(setq uname (substring (shell-command-to-string "uname") 0 -1))
(setq hostname (substring (shell-command-to-string "hostname") 0 -1))
(setq username (substring (shell-command-to-string "whoami") 0 -1))

(setq is-windows-machine (or (string-prefix-p "MINGW32_NT-6.1" uname)
                             (string-prefix-p "MSYS_NT" uname)
                             (string-prefix-p "MINGW64_NT" uname)))

(setq is-macintosh-machine (string= uname "Darwin"))

(setq is-linux-machine (string= uname "Linux"))

;;
;; path & autoloads
;;

(add-to-list 'load-path "~/.emacs.d/lisp")

;; setup packager manager bullshit.
(load "ajt-package-manager")

;; emacsclient can be used to edit files from a terminal
(if window-system
    (progn
      (server-start)
      (add-hook 'server-switch-hook #'raise-frame)))

;; Try saving open buffers
(desktop-save-mode 1)

(require 'thingatpt)

;; custom org-mode keywoars
(setq org-todo-keywords
      '((sequence "TODO" "WIP" "WAIT" "|" "DONE" "PUNT")))

;; some custom git functions
(load "ajt-git")

;; disabled git - prevent emacs from using git, fix index.lock conflicts.
(delete 'Git vc-handled-backends)

;; improvements to grep
(autoload 'ajt-grep-find "ajt-grep-find")

;; swap between .h and .cpp files
(autoload 'ajt-header-swap "ajt-header-swap")

;; new buffers should show up in lower right hand corner
(load "ajt-special-display")

;; irc chat
(autoload 'erc "erc")

;; show entire kill ring in a buffer
(autoload 'browse-kill-ring "browser-kill-ring")

;;
;; major-modes
;;

(autoload 'ruby-mode "ruby-mode")
(autoload 'glsl-mode "glsl-mode")
(autoload 'yaml-mode "yaml-mode")
(autoload 'thrift-mode "thrift-mode")
(autoload 'typescript-mode "typescript-mode")
(autoload 'csharp-mode "csharp-mode")
(autoload 'cuda-mode "cuda-mode") ; from melpa

(add-to-list 'load-path "~/.emacs.d/lisp/lua-mode" t)
(autoload 'lua-mode "lua-mode")
(setq lua-indent-level 4)

(add-to-list 'load-path "~/.emacs.d/lisp/rust-mode" t)
(autoload 'rust-mode "rust-mode")

;; assign modes to file extentions
(setq auto-mode-alist
      (append '(("\\.el\\'" . emacs-lisp-mode)
                ("\\.py\\'" . python-mode)
                ("\\.cpp\\'" . c++-mode)
                ("\\.h\\'" . c++-mode)
                ("\\.hpp\\'" . c++-mode)
                ("\\.cc\\'" . c++-mode)
                ("\\.cu\\'" . c++-mode)
                ("\\.cuh\\'" . c++-mode)
                ("\\.c\\'" . c-mode)
                ("\\.cs\\'" . csharp-mode) ; csharp mode has bugs." c-guess-basic-syntax: Wrong number of arguments: "
                ("\\.rb\\'" . ruby-mode)
                ("\\.dd\\'" . ruby-mode)   ; bbq data definition file
                ("\\.di\\'" . ruby-mode)   ; bbq data instance file
                ("[rR]akefile" . ruby-mode)
                ("\\.yaml\\'" . yaml-mode)
                ("\\.bin\\'" . hexl-mode)  ; binary blob
                ("\\.y\\'" . c-mode)       ; yacc/bison files
                ("\\.l\\'" . c-mode)       ; lex/flex files
                ("\\.glsl\\'" . glsl-mode)
                ("\\.vert\\'" . glsl-mode)
                ("\\.frag\\'" . glsl-mode)
                ("\\.vsh\\'" . glsl-mode)
                ("\\.fsh\\'" . glsl-mode)
                ("\\.slf\\'" . glsl-mode)
                ("\\.slv\\'" . glsl-mode)
                ("\\.slh\\'" . glsl-mode)
                ("\\.js\\'" . js-mode)
                ("\\.qml\\'" . js-mode)
                ("\\.m\\'" . objc-mode)
                ("\\.mm\\'" . objc-mode)
                ("\\.go\\'" . go-mode)
                ("\\.coffee\\'" . coffee-mode)
                ("BROWSE\\'" . ebrowse-tree-mode)
                ("\\.json\\'" . js-mode)
                ("\\.rs\\'" . rust-mode)
                ("COMMIT_EDITMSG" . flyspell-mode)
                ("\\.thrift\\'" . thrift-mode)
                ("\\.lua\\'" . lua-mode))))

;;
;; misc
;;

(defun ajt-init ()
  "Load my init.el file into current buffer"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; revert buffers when they change on disk (except if they are modified)
(setq revert-without-query '("."))
(global-auto-revert-mode)
(setq auto-revert-verbose nil) ;; stfu

;; turn off line wrapping.
(set-default 'truncate-lines t)

;;
;; detects if code buffer uses spaces or tabs for indenting
;; and preseves that setting
;;
(load "ajt-indent")

;; in terminal mode hl-line doesn't show synatax highlights on current line.
;; don't use it at all
(setq ajt-use-hl-line 'nil)

(if ajt-use-hl-line
    (load "hl-line"))

(load "ajt-color-themes")

;;
;; find-file-hook
;;
(add-hook 'find-file-hook
          (lambda ()
            ;; tabs or spaces?
            (ajt-detect-and-set-indentation)
            ;; don't enable line highlight for specific buffer names
            (unless (or (string-match "*ansi-term*" (buffer-name))
                        (string-match "*shell*" (buffer-name))
                        (string-match "*vterm*" (buffer-name))
                        (string-match "*eat*" (buffer-name))
                        (string-match "*ajt-grep*" (buffer-name)))
              ;; turn on line highlight
              (if ajt-use-hl-line
                  (progn
                    (hl-line-mode)
                    (set-face-background hl-line-face ajt-line-color))))))

;; better scroll wheel behavior
(if window-system
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))))

;; no startup message
(setq inhibit-startup-message t)

;; Don't insert instructions in the *scratch* buffer
(setq initial-scratch-message nil)

;; prevent ~ # emacs droppings.
(setq create-lockfiles nil)
(setq auto-save-default nil)

;; hide tool bar & menu
(when window-system
  (tool-bar-mode -1))
(menu-bar-mode -1)


;; enable scroll bars here
(setq ajt-enable-scroll-bars nil)
(if ajt-enable-scroll-bars
    (progn
      (scroll-bar-mode 1)
      (set-scroll-bar-mode 'right))
  (scroll-bar-mode -1))

;; no gutters
(fringe-mode '(0 . 0))

;; no highlighted text when selecting.
(transient-mark-mode 0)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "stroustrup")))

;; create a new *scratch* buffer
(defun scratch-buffer-create nil
  "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
    (setq bufname (concat "*scratch"
  (if (= n 0) "" (int-to-string n))
  "*"))
    (setq n (1+ n))
    (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (if (= n 1) (lisp-interaction-mode))))

;; scrolling output
(setq compilation-scroll-output t)

;; ido find-file & buffer switching is awesome
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq completion-ignored-extensions (cons ".meta" (cons ".d" completion-ignored-extensions)))

;;
;; keybinds
;;

;; KILL overwrite-mode with FIRE
(define-key global-map [(insertchar)] nil)

;; ibuffer is better then buffer-menu
(global-set-key "\C-x\C-b" 'ibuffer)

;; alias M-x with C-x C-m for meta less environments.
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\m" 'execute-extended-command)

;; forward-to-word
(require 'misc)
 ;(global-set-key "\M-f" 'forward-to-word)
(global-set-key "\M-f" 'forward-same-syntax)

;; ham-handed kill-buffer
(global-set-key "\C-x\k" 'kill-buffer)

(global-set-key "\C-x\C-k" 'kill-buffer)

;; cause C-x C-u is just too hard
(global-set-key "\C-z" 'undo)


;; prevent this from invoking suspend-frame, cause it's ANNOYING
(global-set-key "\C-x\C-z" nil)

;; short cut to shell
(global-set-key "\C-c\s" 'ajt-term)
(global-set-key "\C-c\C-s" 'ajt-term)
(global-set-key "\C-c\<SPC>" 'ajt-term)

(defun set-ajt-term-bindings ()
  (local-set-key "\C-c\s" 'ajt-term)
  (local-set-key "\C-c\C-s" 'ajt-term)
  (local-set-key "\C-c\<SPC>" 'ajt-term))

;; make ajt-term work in shell-mode
(add-hook 'shell-mode-hook 'set-ajt-term-bindings)

(global-set-key "\C-\\" 'comment-or-uncomment-region)

;; ham-handed other-window
(global-set-key "\C-x\C-o" 'other-window)
(global-set-key "\C-o" 'other-window)

;; ham-handed find-file
(global-set-key "\C-xf" 'find-file)

;; function key bindings
(global-set-key [f1] 'call-last-kbd-macro)
(global-set-key [f2] 'start-kbd-macro)
(global-set-key [f3] 'end-kbd-macro)
(global-set-key [f4] 'next-error)
(global-set-key [f5] 'ispell-word)
(global-set-key [f6] 'grep-find)
(global-set-key [f7] 'compile)

;; fix for obsolute GREP_OPTIONS spew
(require 'grep)
(grep-apply-setting 'grep-find-command '("unset GREP_OPTIONS; find . -type f -exec grep -nH  {} \\;" . 51))

;; ajt-kill-word
(defun ajt-delete-word (&optional num)
  "Delete from point to beginning of next word"
  (interactive)
  (let ((n (or num 1))
        (start (point)))
    (ajt-forward-word n)
    (delete-region start (point))))
(global-set-key "\M-d" 'ajt-delete-word)

;; ajt-forward-word
(defun ajt-forward-word (&optional num)
  "Move to beginning of next word"
  (interactive)
  (let ((n (or num 1)))
    (forward-same-syntax n)
    (re-search-forward "[\s[:space:]\n]*")))
(global-set-key "\M-f" 'ajt-forward-word)

;; http://stackoverflow.com/questions/145291/smart-home-in-emacs
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key "\C-a" 'smart-beginning-of-line)

;; was mark-whole-buffer
(global-set-key "\C-x\h" 'ajt-header-swap)
(global-set-key "\C-x\C-h" 'ajt-header-swap)

;;
;; trailing whitespace
;;

;; Show trailing whitespace, except for term-mode and compilation-mode
(setq-default show-trailing-whitespace t)

(add-hook 'term-mode-hook
           '(lambda ()
              ;; BROKEN in emacs 24.3 *sigh*
              ;; override ansi-colors                   black     red       green     yellow    blue      magenta   cyan      white
              (setq ansi-term-color-vector [default "#000000" "#963F3C" "#2F9B25" "#9F9D25" "#0042cF" "#FF2180" "#279C9B" "#FFFFFF"])
              (setq show-trailing-whitespace nil)))


(add-hook 'compilation-mode-hook '(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'diff-mode-hook '(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'eat-mode-hook '(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'vterm-mode-hook '(lambda () (setq show-trailing-whitespace nil)))

;;
;; term
;;

;; 't if buffer with name is open
;; nil otherwise
(defun ajt-buffer-open (name)
  (loop for b in (buffer-list) do
        (if (string-equal (buffer-name b) name)
            (return 't))))

;; this works but on windows you have to explictly source ~/.bash_profile
(defun ajt-term ()
  (interactive)
  (if (ajt-buffer-open "*shell*")
      (progn
        (switch-to-buffer-other-window "*shell*"))
      (shell)))

;;
;; ajt-eslint & ajt-jsonlint
;;

(load "ajt-js-helpers")
(load "ajt-py-helpers")
(load "ajt-date-helpers")

;;
;; platform specific stuff
;;

(if is-windows-machine
    (load "ajt-windows-init"))

(if is-macintosh-machine
    (load "ajt-macintosh-init"))

(if is-linux-machine
    (load "ajt-linux-init"))

;; Use vterm on linux
;; NOTE: use vterm-copy-mode (C-c C-t) to move the cursor like shell
(if (string-equal hostname "tony.uthana.dev")
    (use-package vterm :ensure t))
(if (string-equal hostname "GrimTangle.Local")
    (progn
      (use-package eat)
      (setq eat-term-name "xterm-256color")))

;;
;; machine specific projects
;;

(cond
 ;; work dev box
 ((string-equal hostname "tony.uthana.dev")
  (server-start)
  (load "ajt-clipboard-osc52")
  ;;(require 'ajt-term-buddy)
  ;;(ajt-term-buddy-enable-deferred)
  (load "ajt-project-uthana")
  (message "work dev box - linux"))

 ;; home pc - linux
 ((and (string-equal hostname "voidgazer") is-linux-machine)
  ;;(require 'ajt-term-buddy)
  ;;(ajt-term-buddy-enable-deferred)
  ;;(load "ajt-project-taming-camdm")
  (load "ajt-project-uthana-sdk")
  (message "home pc - linux"))

 ;; home pc - windows
 ((and (string-equal hostname "voidgazer") is-windows-machine)
  (load "ajt-project-uthana-sdk")
  (message "home pc - windows"))

 ;; unknown machine
 (t
  (message "Unknown machine")
  ))
