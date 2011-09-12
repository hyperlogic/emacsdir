;; Holy shit I'm editing this file on github.
;; my not so little .emacs.el file

;; which system are we running on.
(setq uname (substring (shell-command-to-string "uname") 0 -1))
(setq hostname (substring (shell-command-to-string "hostname") 0 -1))

;; show line & column in modeline
(setq column-number-mode t)

;; show filesize and % in modeline
(size-indication-mode 1)

;; show time in modeline
(display-time-mode 1)

;; emacsclient can be used to edit files from a terminal
(server-start)

;; add .emacs.d to load path
(setq load-path (cons "~/.emacs.d/" load-path))

;; revert buffers when they change on disk (except if they are modified)
(setq revert-without-query '("."))
(global-auto-revert-mode)
(setq auto-revert-verbose nil) ;; stfu

;; dont jump around so much when scrolling.
;(setq scroll-step 10)

;; default window size
(setq my-window-width 80)
(setq my-window-height 25)

;; color shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Show trailing whitespace.
(setq-default show-trailing-whitespace t)

;; 't if buffer with name is open
;; nil otherwise
(defun ajt-buffer-open (name)
  (loop for b in (buffer-list) do
        (if (string-equal (buffer-name b) name)
            (return 't))))

(setq-default ajt-use-ansi-term 't)
(defun ajt-term ()
  (interactive)
  (if ajt-use-ansi-term
	  (if (ajt-buffer-open "*ansi-term*")
		  (progn
			(switch-to-buffer-other-window "*ansi-term*"))
		(ansi-term "/bin/bash"))
	(if (ajt-buffer-open "*shell*")
		(progn
		  (switch-to-buffer-other-window "*shell*"))
	  (shell))))

;; ruby-mode NOTE: now included in 23.1
(when (< emacs-major-version 23)
  (load-library "ruby-mode"))

;; turn off line wrapping.
(set-default 'truncate-lines t)

;; by default use spaces to indent.
;; NOTE: overridden for ngmoco
(setq-default indent-tabs-mode nil)

;; highlight curent line NOTE: better then hl-line mode
(require 'highlight-current-line)
(highlight-current-line-on t)
;; TODO: move this into color theme.
(set-face-background 'highlight-current-line-face "#101040")

;;
;; Pops up a grep process in a buffer named *ajt-grep*
;;
(defun ajt-grep-find-shell-cmd (cmd)
  ;; echo the cmd line to the *Message* log
  (message cmd)

  ;; exec command in a new process
  (start-process-shell-command "ajt-grep" "*ajt-grep*" cmd)
  (pop-to-buffer "*ajt-grep*")
  (compilation-mode))

(defun ajt-filter-include-patterns (patterns)
  (remq nil (mapcar (lambda (x) (if (string-equal (substring x 0 1) "!") nil x)) patterns)))

(defun ajt-concat-include-patterns (type patterns)
  (let ((include-patterns (ajt-filter-include-patterns patterns)))
	(if include-patterns
		(concat "\\( " (mapconcat (lambda (x) (concat type " \"" x "\"")) include-patterns " -or ") " \\) ")
	  "")))

(defun ajt-filter-exclude-patterns (patterns)
  (remq nil (mapcar (lambda (x) (if (string-equal (substring x 0 1) "!") (substring x 1) nil)) patterns)))

(defun ajt-concat-exclude-patterns (type patterns)
  (let ((exclude-patterns (ajt-filter-exclude-patterns patterns)))
	(if exclude-patterns
		(concat "-not \\( " (mapconcat (lambda (x) (concat type " \"" x "\"")) exclude-patterns " -or ") " \\) ")
	  "")))

;;
;; Custom grep search
;;
(defun ajt-grep-find (search-term path-patterns name-patterns)
  "Passes the string SEARCH-TERM to grep
SEARCH-TERM regex to search for (passed to grep)
PATH-PATTERNS is a list of find style patterns that must match full path.  If pattern starts with a ! character then it can be used to exclude a path.
NAME-PATTERNS is a list of find style patterns that must match base filename.  If pattern starts with a ! character then it can be used to exclude a file.
For example:  
  (ajt-grep-find \"main\" '(\"d:/tras/cdc/runtime\" \"d:/tras/code/game\") '(\"*.cpp\" \"*.h\" \"*.c\")))"

  (let* ((path-include-string (mapconcat 'identity (ajt-filter-include-patterns path-patterns) " "))
		 (path-exclude-string (ajt-concat-exclude-patterns "-path" path-patterns))
		 (name-include-string (ajt-concat-include-patterns "-name" name-patterns))
		 (name-exclude-string (ajt-concat-exclude-patterns "-name" name-patterns))
		 (cmd (make-string 0 ?x)))

	(setq cmd (concat cmd "find " path-include-string))
	(when (not (string-equal path-exclude-string ""))
	  (setq cmd (concat cmd " " path-exclude-string)))
	(when (not (string-equal name-include-string ""))
	  (setq cmd (concat cmd " -and " name-include-string)))
	(when (not (string-equal name-exclude-string ""))
	  (setq cmd (concat cmd " -and " name-exclude-string)))

	(setq cmd (concat cmd " -type f -exec grep -nH -e " search-term " {} /dev/null \\;"))

	;; send cmd to *Messages*
	(message cmd)

	;(message (format "path-include-patterns = %S" path-include-string))
	;(message (format "path-exclude-patterns = %S" path-exclude-string))
	;(message (format "name-include-patterns = %S" name-include-string))
	;(message (format "name-exclude-patterns = %S" name-exclude-string))

	(ajt-grep-find-shell-cmd cmd)))

;;
;; bluesliver - home laptop
;;
(if (string= "Darwin" uname)
    (progn

      ;; go stuff
;      (add-to-list 'load-path "~/go/misc/emacs/" t)
;      (require 'go-mode-load)

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
      ;(setq-default line-spacing 2) ; so the bottoms of ] and } don't get cut off

      ;; Textmate style text (23.1)
      (set-face-attribute 'default nil :family "Monaco" :height 120)
      (setq my-window-width 200)
      (setq my-window-height 56)

      ;; for tired eyes
      ;(set-face-attribute 'default nil :family "Monaco" :height 140)

      ;; Menlo (modified Bitstream Vera Sans Mono)
      ;(set-face-attribute 'default nil :family "Menlo" :height 115)
      ;(setq my-window-width 200)
      ;(setq my-window-height 60)

      ;; Proggy clean
      ;(set-face-attribute 'default nil :family "ProggyCleanTTSZ" :height 155)
      ;(setq my-window-width 200)
      ;(setq my-window-height 60)

      ;; use command key as meta
      (setq mac-command-modifier 'meta)
      (setq compile-command (concat "cd ~/code/lavender/; rake debug"))

      ;; For CinemaDisplay, try to only have 4 buffers at once
      (setq split-width-threshold 400)
      (setq split-height-threshold 200)

      ;; work @ ngmoco:)
      (if (or (string= "Anthony-Thibault_MacBook-Pro.local" hostname)
              (string= "dhcp-101.corp.ngmoco.com" hostname))
          (progn

			(setq mac-allow-anti-aliasing t)
			(set-face-attribute 'default nil :family "Monaco" :height 115)
			;(setq-default line-spacing 0) ; so the bottoms of ] and } don't get cut off

            ;; uhh, only useful when screen is maximized
            ;(ns-toggle-fullscreen)

            ;; ngmoco uses tabs! omg
            (setq-default indent-tabs-mode 't)

            ;; use aspell
            (setq-default ispell-program-name "aspell")

            ;; WebGame javascript search with regex
            (defun ajt-js-search (arg)
              "Search for a regex in all ngCore javascript files"
              (interactive "sngcore-js:")
              (ajt-grep-find arg '("~/WebGame") '("*.js" "!application.js")))

            ;; WebGame cpp search with regex
            (defun ajt-cpp-search (arg)
              "Search for a regex in all ngCore cpp files"
              (interactive "sngcore-cpp:")
              (ajt-grep-find arg '("~/WebGame" "!/Users/athibault/WebGame/android/jni/utils/v8/*") '("*.cc" "*.cpp" "*.h" "*.mm" "*.m")))

            ;; WebGame java search with regex
            (defun ajt-java-search (arg)
              "Search for a regex in all ngCore java files"
              (interactive "sngcore-java:")
              (ajt-grep-find arg '("~/WebGame") '("*.java")))

			;; launch gamejs
			(defun ajt-arun ()
			  (interactive)
			  (save-some-buffers)
			  (shell-command "adb shell am broadcast -a com.ngmoco.gamejs.STOP > /dev/null" nil)
			  (shell-command "adb shell am start -a com.ngmoco.gamejs.RUN -e nativeLog true > /dev/null" nil)
			  (pop-to-buffer "*ajt-logcat*"))

			;(load-library "emacs-android")

			(defun ajt-logcat ()
			  (pop-to-buffer "*ajt-logcat*")
			  (shell-command "adb logcat&" "*ajt-logcat*"))

            (defun ajt-narwhal ()
              (interactive)
			  (if (get-buffer "*ajt-narwhal*") (kill-buffer (get-buffer "*ajt-narwhal*")))
			  (message "stopping game")
			  (shell-command "adb shell am broadcast -a com.ngmoco.gamejs.STOP > /dev/null" nil)
			  (message "building narwhal...")
              (if (not (equal 0 (shell-command "cd ~/WebGame/submodules/narwhal/Bundles/MobageBoot/; make dev" "*ajt-narwhal*")))
				  (progn
					(pop-to-buffer "*ajt-narwhal*")
					(compilation-mode)
					(error "narwhal build failed"))
				(kill-buffer "*ajt-narwhal*"))
			  (message "starting game")
			  (shell-command "adb shell am start -a com.ngmoco.gamejs.RUN -e nativeLog true > /dev/null" nil)
			  (pop-to-buffer "*ajt-logcat*"))


            ;; key bindings
            (global-set-key [f8] 'ajt-js-search)
            (global-set-key [f9] 'ajt-cpp-search)
			(global-set-key [f10] 'ajt-java-search)
			(global-set-key [f11] 'ajt-narwhal)

            (setq compile-command (concat "cd ~/WebGame/; make afast; make arun game=Samples/ajt/RenderTargetTest"))

			;; make tags
			(defun ajt-make-tags ()
			  "Make TAGS"
			  (interactive)
			  (ajt-grep-find-shell-cmd "etagsGen.rb ~/WebGame/TAGS ~/WebGame"))

			;; use TAGS file in these dirs.
			(setq tags-table-list '("~/WebGame"))

			;; it was cute, for 5 seconds.
			(setq load-path (cons "~/.emacs.d/nyan-mode" load-path))
			(load-library "nyan-mode")
			;(nyan-mode)


            ))
      ))

(if (not (equal 0 (shell-command "true")))
	(message "ERRORS!")
  (message "NO ERRORS!"))

;;
;; vivisect - arch home pc
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
;; windows (work) - crystal dynamics
;;
(if (string= hostname "RWCWRK_7001233")
    (progn

      ;; set window size
      ;(set-face-attribute 'default nil :family "courier new" :height 105)
      (set-face-attribute 'default nil :family "courier new" :height 90)
      (setq my-window-width 195)
      (setq my-window-height 63)

      ;; set-up build
      (setq compile-command "cd D:\\tras& easymake xenon_release -noopts")

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

      ;; Search all animation state machines and piplelines (SLOW!)
      (defun ajt-anim-graph-search (arg)
        "search for a regex in all animation state machines and pipelines"
        (interactive "sgrep-regexp:")
        (ajt-grep-find arg '("d:/tras/animation/animgraph") '("*.animstate" "*.animpipe")))

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

;; Don't insert instructions in the *scratch* buffer
(setq initial-scratch-message nil)

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

;; no scroll bars
(scroll-bar-mode -1)

;; hide gutters
;(fringe-mode nil)

;; color-theme
(setq load-path (cons "~/.emacs.d/color-theme-6.6.0" load-path))
(require 'color-theme)
(load "ajt-color-themes.el")
(color-theme-initialize)

(cond (window-system
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

;; short cut to shell
(global-set-key "\C-c\s" 'ajt-term)
(global-set-key "\C-c\C-s" 'ajt-term)
(global-set-key "\C-c\<SPC>" 'ajt-term)

;; C-\ is indent-region
;(global-set-key "\M-C-i" 'indent-region)
;(global-set-key [C-c C-c] 'comment-or-uncomment-region)
;(global-set-key [C-c C-u] 'uncomment-region)
;(global-set-key "\M-s" 'grep-find)

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
(setq ajt-special-buffers `("*compilation*" "*grep*" "*shell*" "*ajt-grep*" "*ansi-term*" "*ajt-logcat*"))

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

(defun ajt-split-window-trirds()
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

;; js2-mode
;; NOTE: needs to be byte compiled first
;; emacs --batch --eval '(byte-compile-file "~/.emacs.d/js2.el")'
;(load-library "js2")

;; lua-mode
(require 'lua-mode)
(setq lua-indent-level 4)

;; assign modes to file extentions
(setq auto-mode-alist
      (append '(("\\.cpp\\'" . c++-mode)
                ("\\.h\\'" . c++-mode)
                ("\\.c\\'" . c-mode)
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
                ("\\.js\\'" . js-mode)
                ("\\.m\\'" . objc-mode)
                ("\\.mm\\'" . objc-mode)
                ("\\.dtp\\'" . xml-mode)
                ("\\.dtpinc\\'" . xml-mode)
                ("\\.go\\'" . go-mode)
                ("BROWSE\\'" . ebrowse-tree-mode)
                ("\\.lisp\\'" . common-lisp-mode)
                ("\\.json\\'" . js-mode)
                ("\\.jake\\'" . js-mode)
                ("[jJ]akefile" . js-mode)
                (".boot[Cc]onfig" . js-mode)
                ("COMMIT_EDITMSG" . flyspell-mode))
              auto-mode-alist))

;; irc chat
(require 'erc)

;; (setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
;;                                   interpreter-mode-alist))

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

;; list of header and soruce file extentions
(setq ajt-hdr-ext-list `(".h" ".hpp" ".vsh"))
(setq ajt-src-ext-list `(".cpp" ".c" ".m" ".mm" ".fsh"))

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

;; Perforce edit
(defun ajt-p4-edit ()
  "Checkout the current buffer"
  (interactive)
  (let ((cmd (format "/usr/local/bin/p4 -p rwcp4d02.eidos.com:1666 -c tthibault_laptop edit %s" (buffer-file-name (current-buffer)))))
    (shell-command cmd))
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

;; forth mode
(autoload 'forth-mode "gforth.el")
(autoload 'forth-block-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.fs$" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fth$" . forth-mode))

;; markdown mode
(autoload 'markdown-mode "markdown-mode.el")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

