;; my not so little .emacs.el file

;; which system are we running on.
(setq uname (substring (shell-command-to-string "uname") 0 -1))
(setq hostname (substring (shell-command-to-string "hostname") 0 -1))

;; show line & column in status
(setq column-number-mode t)

;; So I can use emacsclient to edit files from a terminal
(server-start)

;; add .emacs.d to load path
(setq load-path (cons "~/.emacs.d/" load-path))

;; revert buffers when they change on disk (except if they are modified)
(setq revert-without-query '("."))

;; automatically revert buffers if they change on disk
(global-auto-revert-mode)

;; dont jump around so much when scrolling.
;(setq scroll-step 10)

;; default window size
(setq my-window-width 80)
(setq my-window-height 25)

;; color shell
;(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; ruby-mode NOTE: now included in 23.1
(when (< emacs-major-version 23)
  (load-library "ruby-mode"))

;; FAIL!
;;
;; use ctrl-meta n and p to move between windows i.e. buffers
;(global-set-key (kbd "C-M-n") 'next-multiframe-window)
;(global-set-key (kbd "C-M-p") 'previous-multiframe-window)
;
;; can't use these at init *sigh*
;
;; ruby-mode shadows this global binding, so redefine it.
;(define-key ruby-mode-map (kbd "C-M-n") 'next-multiframe-window)
;(define-key ruby-mode-map (kbd "C-M-p") 'previous-multiframe-window)
;; xml-mode has the same problem
;(define-key nxml-mode-map (kbd "C-M-n") 'next-multiframe-window)
;(define-key nxml-mode-map (kbd "C-M-p") 'previous-multiframe-window)

;; bluesliver
(if (string= "Darwin" uname)
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
	  (set-face-attribute 'default nil :family "Monaco" :height 110)
	  (setq my-window-width 200)
	  (setq my-window-height 56)

	  ;; Bitstream Vera Sans Mono 
	  ;(set-face-attribute 'default nil :family "Bitstream Vera Sans Mono" :height 130)
	  ;(setq my-window-width 200)
	  ;(setq my-window-height 60)

	  ;; use command key as meta
	  (setq mac-command-modifier 'meta)
	  (setq compile-command (concat "cd ~/code/lacquer/src/; rake debug"))

	  ;; For CinemaDisplay, try to only have 4 buffers at once
	  (setq split-width-threshold 400)
	  (setq split-height-threshold 100)

	  ))

;; vivisect
(if (string= "vivisect" hostname)
    (progn
      (set-face-attribute 'default nil :family "Bitstream Vera Sans Mono" :height 85)
	  (setq split-width-threshold 200)
	  ))

;; windows (home)
(if (string= uname "MINGW32_NT-5.1")
	(progn
	  (set-face-attribute 'default nil :family "courier new" :height 100)
	  (setq my-window-width 154)
	  (setq my-window-height 60)))

;; windows (work)
(if (string= hostname "RWCWRK_7001077")
    (progn
      ;; turn off line wrapping.
      (set-default 'truncate-lines t)

      ;; set window size
      (set-face-attribute 'default nil :family "consolas" :height 110)
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

      (defun ajt-dtp ()
		"Dired dtp dir"
		(interactive)
		(find-file "D:/TRAS/dtp"))

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
		(start-process-shell-command "my-process" "*dtp-search*"
									 (concat "find d:/tras/cdc/dtp d:/tras/code/dtp ( -name \"*.dtp\" -o -name \"*.dtpinc\" ) -type f -print0 | xargs -0 -e grep -nH -e " arg))
		(pop-to-buffer "*dtp-search*")
		(compilation-mode))

	  ;; Search all code directories with a regex
	  (defun ajt-code-search (arg)
		"search for a regex in all code files"
		(interactive "sgrep-regexp:")
		(start-process-shell-command "my-process" "*code-search*"
									 (concat "find d:/tras/cdc/runtime d:/tras/code/game/ ( -name \"*.cpp\" -o -name \"*.h\" ) -type f -print0 | xargs -0 -e grep -nH -e " arg))
		(pop-to-buffer "*code-search*")
		(compilation-mode))

	  (defun ajt-graph-search (arg)
		"search for a regex in all graph dat files"
		(interactive "sgrep-regexp:")
		(start-process-shell-command "my-process" "*code-search*"
									 (concat "find d:/tras/area/ d:/tras/object/ d:/tras/design/ ( -name \"*.dat\" -o -name \"*.admd\" -o -name \"*.ags\" ) -type f -print0 | xargs -0 -e grep -nH -e " arg))
		(pop-to-buffer "*code-search*")
		(compilation-mode))
	  

	  ;; use TAGS file in these dirs.
	  (setq tags-table-list '("~/.emacs.d/etags"))))

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

;; compile
(global-set-key [f7] 'compile)
(global-set-key [f9] 'compile)

;; kbd-macros
(global-set-key [f1] 'call-last-kbd-macro)
(global-set-key [f2] 'start-kbd-macro)
(global-set-key [f3] 'end-kbd-macro)
(global-set-key [f4] 'next-error)

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
(setq ajt-special-buffers `("*compilation*" "*grep*" "*shell*"))

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
  "Always split the lower right most window"
  (split-window (ajt-lr-window (window-list))))

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

;; other-frame
(global-set-key [f5] 'other-frame)

;; prevent this from invoking suspend-frame, cause it's ANNOYING
(global-set-key "\C-x\C-z" nil)

;; glsl-mode
(load-library "glsl-mode")

;; yaml-mode
(load-library "yaml-mode")



;; assign modes to file extentions
(setq auto-mode-alist
      (append '(("\\.cpp$" . c++-mode)
				("\\.h$" . c++-mode)
				("\\.c$" . c-mode)
			    ("\\.rb$" . ruby-mode)
				("\\.dd$" . ruby-mode)   ; bbq data definition file
				("\\.di$" . ruby-mode)   ; bbq data instance file
				("\\.yaml$" . yaml-mode)
				("\\.bin$" . hexl-mode)  ; binary blob
				("\\.y$" . c-mode)       ; yacc/bison files
				("\\.l$" . c-mode)       ; lex/flex files
				("\\.glsl$" . glsl-mode)
				("\\.vert\\'" . glsl-mode)
				("\\.frag\\'" . glsl-mode)
				("\\.m$" . objc-mode)
				("\\.mm$" . objc-mode)
				("\\.dtp$" . xml-mode)
				("\\.dtpinc$" . xml-mode)
				("\\.go$" . go-mode)
				("\BROWSE$" . ebrowse-tree-mode))
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

