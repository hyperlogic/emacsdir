;; my not so little .emacs.el file

;; which system are we running on.
(setq uname (substring (shell-command-to-string "uname") 0 -1))
(setq hostname (substring (shell-command-to-string "hostname") 0 -1))

;; show line & column in status
(setq column-number-mode t)

;; emacsclient can be used to edit files from a terminal
(server-start)

;; add .emacs.d to load path
(setq load-path (cons "~/.emacs.d/" load-path))

;; revert buffers when they change on disk (except if they are modified)
(setq revert-without-query '("."))
(global-auto-revert-mode)

;; dont jump around so much when scrolling.
;(setq scroll-step 10)

;; default window size
(setq my-window-width 80)
(setq my-window-height 25)

;; color shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; ruby-mode NOTE: now included in 23.1
(when (< emacs-major-version 23)
  (load-library "ruby-mode"))

;; turn off line wrapping.
(set-default 'truncate-lines t)

;; Pops up a grep process in a buffer named *ajt-grep*
;; For example: 
;;   (ajt-grep-find "main" '("d:/tras/cdc/runtime" "d:/tras/code/game") '("*.cpp" "*.h" "*.c")))
;;
(defun ajt-grep-find (search-term search-paths file-globs)
  "Passes the string term to grep as the search term
   search-paths is a list of directory strings to search in,
   file-globs is a list of glob strings."
  (let ((paths (mapconcat 'identity search-paths " "))
		(extns (mapconcat (lambda (x) (concat "\"" x "\"")) file-globs " -o -name ")))
	(start-process-shell-command "ajt-grep" "*ajt-grep*" (concat "find " paths " ( -name " extns " ) -type f -print0 | xargs -0 -e grep -nH -e " search-term))
	(pop-to-buffer "*ajt-grep*")
	(compilation-mode)))

;;
;; bluesliver
(if (string= "bluesilver.local" hostname)
	(progn
      
	  ;; go stuff
	  (add-to-list 'load-path "~/go/misc/emacs/" t)
	  (require 'go-mode-load)

	  ;; turn off anti-aliasing
	  ;(setq mac-allow-anti-aliasing nil)

	  ;; turn on anti-aliasing
	  (setq mac-allow-anti-aliasing t)

	  ;; Note: be sure to also enter the following into the shell
	  ;; defaults write org.gnu.Emacs AppleAntiAliasingThreshold 128

	  ;; tiny xcode font
	  ;(set-face-attribute 'default nil :family "Monaco" :height 100)
	  ;(setq my-window-width 234)
	  ;(setq my-window-height 71)

	  ;; Textmate style text (23.1)
	  (set-face-attribute 'default nil :family "Monaco" :height 120)
	  (setq my-window-width 200)
	  (setq my-window-height 56)

	  ;; Menlo (modified Bitstream Vera Sans Mono)
	  ;(set-face-attribute 'default nil :family "Menlo" :height 115)
	  ;(setq my-window-width 200)
	  ;(setq my-window-height 60)

	  ;; Proggy clean
	  ;(set-face-attribute 'default nil :family "ProggyCleanTTSZ" :height 160)
	  ;(setq my-window-width 200)
	  ;(setq my-window-height 60)

	  ;; use command key as meta
	  (setq mac-command-modifier 'meta)
	  (setq compile-command (concat "cd ~/code/iphone/circull/; rake release"))

	  ;; For CinemaDisplay, try to only have 4 buffers at once
	  (setq split-width-threshold 400)
	  (setq split-height-threshold 100)

	  ))

;;
;; vivisect
;;
(if (string= "vivisect" hostname)
    (progn
	  ;(set-face-attribute 'default nil :family "inconsolata" :height 105)
	  (set-face-attribute 'default nil :family "courier" :height 87)

	  ;; dont be splittin my windows up
	  (setq split-width-threshold 200)

	  ;; put scroll bar on right
	  (set-scroll-bar-mode 'right)
	  ))

;;
;; windows (home)
;;
(if (string= uname "MINGW32_NT-6.1")
	(progn
	  (set-face-attribute 'default nil :family "courier new" :height 100)
	  (setq my-window-width 154)
	  (setq my-window-height 60)))

;;
;; windows (work)
;;
(if (string= hostname "RWCWRK_7001233")
    (progn

      ;; set window size
      ;(set-face-attribute 'default nil :family "courier new" :height 105)
	  (set-face-attribute 'default nil :family "courier new" :height 90)
      (setq my-window-width 195)
      (setq my-window-height 63)

      ;; set-up build
      (setq compile-command "cd D:\\tras& easymake xenon_debug")

	  ;; ispell not available on windows
	  (setq-default ispell-program-name "aspell")

      ;;
      ;; Open a Dired buffer in common TRAS directories
      ;;
      (defun ajt-cdc-code ()
		"Dired cdc code dir"
		(interactive)
		(find-file "D:/TRAS/cdc/runtime"))

      (defun ajt-game-code ()
		"Dired game code dir"
		(interactive)
		(find-file "D:/TRAS/code/game"))

      (defun ajt-cdc-dtp ()
		"Dired cdc dtp dir"
		(interactive)
		(find-file "D:/TRAS/cdc/dtp"))

      (defun ajt-code-dtp ()
		"Dired code dtp dir"
		(interactive)
		(find-file "D:/TRAS/code/dtp"))

	  ;; build TAGS & BROWSE file for all game code
	  (defun ajt-build-tags ()
		(interactive)
		(shell-command "c:& cd %HOME%/.emacs.d/ebrowse/& ruby makefiles.rb")
		(shell-command "c:& cd %HOME%/.emacs.d/etags/& ruby makefiles.rb"))

	  ;; run class browser
      (defun ajt-browse ()
		"open up ebrowser"
		(interactive)
		(find-file "~/.emacs.d/ebrowse/BROWSE"))

	  ;; Search all dtp directries with a regex
	  (defun ajt-dtp-search (arg)
		"search for a regex in all dtp files"
		(interactive "sgrep-regexp:")
		(ajt-grep-find arg '("d:/tras/cdc/dtp" "d:/tras/code/dtp") '("*.dtp" "*.dtpinc")))

	  ;; Search all code directories with a regex
	  (defun ajt-code-search (arg)
		"search for a regex in all code files"
		(interactive "sgrep-regexp:")
		(ajt-grep-find arg '("d:/tras/cdc/runtime" "d:/tras/code/game") '("*.cpp" "*.h" "*.c")))

	  ;; Search all action graphs (SLOW!)
	  (defun ajt-graph-search (arg)
		"search for a regex in all graph dat files"
		(interactive "sgrep-regexp:")
		(ajt-grep-find arg '("d:/tras/area" "d:/tras/object" "d:/tras/design") '("*.dat" "*.admd" "*.ags")))

	  ;; Search all object dat files (SLOW!)
	  (defun ajt-object-search (arg)
		"search for a regex in all object dat files"
		(interactive "sgrep-regexp:")
		(ajt-grep-find arg '("d:/tras/object") '("*.dat")))

	  ;; Search all script package files (SLOW!)
	  (defun ajt-script-search (arg)
		"search for a regex in all script package files"
		(interactive "sgrep-regexp:")
		(ajt-grep-find arg '("d:/tras/area" "d:/tras/object") '("*.package")))

	  ;; use TAGS file in these dirs.
	  (setq tags-table-list '("~/.emacs.d/etags"))

	  ;; For try to only have 4 buffers at once
	  (setq split-width-threshold 400)
	  (setq split-height-threshold 100)

	  ))

;; main frame
(setq initial-frame-alist
      `((top . 0)
        (left . 0)
		(width . ,my-window-width)
		(height . ,my-window-height)))


;; no startup message
(setq inhibit-startup-message t)

;; prevent ~ emacs droppings.
;; Change backup behavior to save in a directory, not in a miscellany
;; of files all over the place.
(setq backup-by-copying t
	  backup-directory-alist '(("." . "~/.emacs.d/backups"))
	  delete-old-versions t
	  kept-new-versions 6
	  kept-old-versions 2
	  version-control t)

;; hide tool bar & menu
(when window-system
  (tool-bar-mode -1))
(menu-bar-mode -1)

;; color-theme
(setq load-path (cons "~/.emacs.d/color-theme-6.6.0" load-path))
(require 'color-theme)
(load "ajt-color-themes.el")
(color-theme-initialize)

(cond ((window-system)
	   (color-theme-ajt-no-bold-blue-sea))
	  (t
	   (color-theme-charcoal-black)))

;; syntax highlighting for c++
(setq c-basic-offset 4)
(setq tab-width 4)
(setq default-tab-width 4)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "stroustrup")))

;; create a new *scratch* buffer
(defun create-scratch-buffer nil
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

(defun scroll-down-keep-cursor () 
   "Scroll the text one line down while keeping the cursor"
   (interactive) 
   (scroll-down 1)) 

(defun scroll-up-keep-cursor () 
   "Scroll the text one line up while keeping the cursor"
   (interactive) 
   (scroll-up 1)) 

;(global-set-key "\M-p" 'scroll-down-keep-cursor)
;(global-set-key "\M-n" 'scroll-up-keep-cursor)

;(global-set-key "\M-n" 'scroll-up)
;(global-set-key "\M-p" 'scroll-down)

;; open buffer-menu (list buffers)
;(global-set-key "\C-x\C-b" 'buffer-menu)
;(global-set-key "\C-l" 'buffer-menu)

;; alias M-x with C-x C-m for meta less environments.
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\m" 'execute-extended-command)

;; ham-handed undo
;;(global-set-key "\C-x\C-u" 'undo)

;; cause C-x C-u is just too hard
(global-set-key "\C-z" 'undo)

;; short cut to shell
(global-set-key "\C-c\s" 'shell)
(global-set-key "\C-c\C-s" 'shell)

;; C-\ is indent-region
;(global-set-key "\M-C-i" 'indent-region)
;(global-set-key [C-c C-c] 'comment-region)
;(global-set-key [C-c C-u] 'uncomment-region)
;(global-set-key "\M-s" 'grep-find)

;; ham-handed other-window
(global-set-key "\C-x\C-o" 'other-window)

;; ham-handed find-file
(global-set-key "\C-xf" 'find-file)

;; M-$ doesn't work on Mac OS X so move to C-$ instead
(global-set-key (kbd "C-$") 'ispell-word)

;; function key bindings
(global-set-key [f1] 'call-last-kbd-macro)
(global-set-key [f2] 'start-kbd-macro)
(global-set-key [f3] 'end-kbd-macro)
(global-set-key [f4] 'next-error)
(global-set-key [f5] 'ispell-word)
(global-set-key [f6] 'grep-find)
(global-set-key [f7] 'compile)
(global-set-key [f8] 'ajt-code-search)
(global-set-key [f9] 'ajt-dtp-search)

;; scrolling output
(setq compilation-scroll-output t)

;; ido find-file & buffer switching is awesome
(require 'ido)
(ido-mode t)

;; all windows should be pop-up.
;; NOTE: specifically this allows the "*shell*" buffer to go through the normal display-buffer logic.
;; which allows me to treat it specially below.
(setq same-window-buffer-names nil)

;;
;; I frequently use compile, grep and shell windows.
;; However during long sessions I find it annoying to have to shuffle and kill these windows all the time.
;; In an attempt to prevent this I tag these windows as "special".
;;
;; When "special" buffers are displayed they should appear in the lower right side of the frame.
;; Unless, there is already an existing "special" buffer being displayed, if so then use that window instead.
;;

;; list of "special" buffers, add new ones here.
(setq ajt-special-buffers `("*compilation*" "*grep*" "*shell*" "*ajt-grep*"))

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

;; TODO: pay attention to the split-height-threshold etc.
(defun ajt-split-window-preferred-function (x)
  "Always split the lower right most window, unless there is only one window."
  "In that case split it horizontaly"
  (if (= (length (window-list)) 1)
	  (split-window-horizontally)
	(split-window (ajt-lr-window (window-list)))))

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

;;
;; end special display
;;

;; other-frame (I don't really work with multiple monitors much anymore)
;(global-set-key [f5] 'other-frame)

;; prevent this from invoking suspend-frame, cause it's ANNOYING
(global-set-key "\C-x\C-z" nil)

;; glsl-mode
(load-library "glsl-mode")

;; yaml-mode
(load-library "yaml-mode")

;; assign modes to file extentions
(setq auto-mode-alist
      (append '(("\\.cpp\\'" . c++-mode)
				("\\.h\\'" . c++-mode)
				("\\.c\\'" . c-mode)
			    ("\\.rb\\'" . ruby-mode)
				("\\.dd\\'" . ruby-mode)   ; bbq data definition file
				("\\.di\\'" . ruby-mode)   ; bbq data instance file
				("\\.yaml\\'" . yaml-mode)
				("\\.bin\\'" . hexl-mode)  ; binary blob
				("\\.y\\'" . c-mode)       ; yacc/bison files
				("\\.l\\'" . c-mode)       ; lex/flex files
				("\\.glsl\\'" . glsl-mode)
				("\\.vert\\'" . glsl-mode)
				("\\.frag\\'" . glsl-mode)
				("\\.m\\'" . objc-mode)
				("\\.mm\\'" . objc-mode)
				("\\.dtp\\'" . xml-mode)
				("\\.dtpinc\\'" . xml-mode)
				("\\.go\\'" . go-mode)
				("BROWSE\\'" . ebrowse-tree-mode)
				("\\.lisp\\'" . common-lisp-mode))
			  auto-mode-alist))

;; irc chat
(require 'erc)

;; (setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
;;									 interpreter-mode-alist))

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))

;; count words in region
(defun count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  (message "Counting words in region ... ")

;;; 1. Set up appropriate conditions.
  (save-excursion
    (let ((count 0))
      (goto-char beginning)

;;; 2. Run the while loop.
      (while (and (< (point) end)
                  (re-search-forward "\\w+\\W*" end t))
        (setq count (1+ count)))

;;; 3. Send a message to the user.
      (cond ((zerop count)
             (message
              "The region does NOT have any words."))
            ((= 1 count)
             (message
              "The region has 1 word."))
            (t
             (message
              "The region has %d words." count))))))


;; describe-unbound-keys
(load-library "unbound")

(defun dos2unix ()
   (interactive)
   (beginning-of-buffer)
   (while 
       (search-forward "\r") 
     (replace-match "")))


(defun ajt-init ()
  "Load my init.el file into current buffer"
  (interactive)
  (find-file "~/.emacs.d/init.el"))


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

;; Swap between .h and .cpp files
;; TODO: support for c, Objective-C & Objective-C++
(defun ajt-header-swap ()
  "Swap between .h & .cpp files"
  (interactive)
  (let* ((filename (buffer-file-name (current-buffer)))
		 (ext (file-name-extension filename))
		 (cpp (concat (file-name-sans-extension filename) ".cpp"))
		 (hdr (concat (file-name-sans-extension filename) ".h")))
	(cond
	 ((and (string= ext "h") (file-exists-p cpp))
	  (find-file cpp))
	 ((and (string= ext "cpp") (file-exists-p hdr))
	  (find-file hdr))
	 ((string= ext "h")
	  (error "could not find \"%s\"" cpp))
	 ((string= ext "cpp")
	  (error "could not find \"%s\"" h))
	 ('t
	  (error "not a .h or .cpp file")))))

;; was mark-whole-buffer
(global-set-key "\C-x\h" 'ajt-header-swap)

;; Perforce edit
(defun ajt-p4-edit ()
  "Checkout the current buffer"
  (interactive)
  (shell-command (format "p4 edit %s" (buffer-file-name (current-buffer))))
  (revert-buffer))

(defun ajt-chmod-writeable ()
  "make current buffer rw-rw-rw-"
  (interactive)
  (chmod (buffer-file-name (current-buffer)) 666)
  (revert-buffer))
  
(defun ajt-chmod-read-only ()
  "make current buffer read-only r--r--r--"
  (interactive)
  (chmod (buffer-file-name (current-buffer)) 444)
  (revert-buffer))

;; for twiki page editing M-x erin-mode
;;(require 'erin)

;; no highlighted text when selecting.
(transient-mark-mode nil)

;; slime for Clozure Lisp
;(set-language-environment "utf-8")
;(add-to-list 'load-path "~/install/slime-2010-07-01/")
;(setq inferior-lisp-program "/opt/local/bin/ccl64 -K utf-8")
;(require 'slime)
;(setq slime-net-coding-system 'utf-8-unix)
;(slime-setup '(slime-fancy))