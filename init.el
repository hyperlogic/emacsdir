;; my little .emacs.el file

;; which system are we running on.
(setq uname (substring (shell-command-to-string "uname") 0 -1))
(setq host (substring (shell-command-to-string "hostname") 0 -1))

;; show line & column in status
(setq column-number-mode t)

;; color shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; works with emacs client
(server-start)

;; add .emacs.d to load path
(setq load-path (cons "~/.emacs.d/" load-path))

;; revert buffers when they change on disk (except if they are modified)
(setq revert-without-query '("."))

;; automatically revert buffers if they change on disk
(global-auto-revert-mode)

;; dont jump around so much when scrolling.
(setq scroll-step 10)

;; defaults
(setq my-window-width 80)
(setq my-window-height 25)


;; macbook
(if (string= "Darwin" uname)
	(progn

	  ;; go stuff
	  (add-to-list 'load-path "~/go/misc/emacs/" t)
	  (require 'go-mode-load)

	  ;; turn off anti-aliasing
	  ;(setq mac-allow-anti-aliasing nil)

	  ;; Note: be sure to also enter the following into the shell
	  ;; defaults write org.gnu.Emacs AppleAntiAliasingThreshold 128

	  ;; tiny xcode font
	  ;(set-face-attribute 'default nil :family "Monaco" :height 100)
	  ;(setq my-window-width 234)
	  ;(setq my-window-height 70)

	  ;; Textmate style text
	  (set-face-attribute 'default nil :family "Monaco" :height 120)
	  (setq my-window-width 200)
	  (setq my-window-height 60)

	  ;; vt220
	  ;(set-face-attribute 'default nil :family "Glass_TTY_VT220" :height 200)
	  ;(setq my-window-width 200)
	  ;(setq my-window-height 70)

	  ;; use command key as meta
	  (setq mac-command-modifier 'meta)
	  (setq compile-command (concat "cd ~/code/lacquer/src/; rake debug"))))

;; windows (home)
(if (string= uname "MINGW32_NT-5.1")
	(progn
	  (set-face-attribute 'default nil :family "courier new" :height 100)
	  (setq my-window-width 154)
	  (setq my-window-height 60)))

;; windows (work)
(if (string= host "RWCWRK_7001077")
    (progn
      ;; turn off line wrapping.
      (set-default 'truncate-lines t)

      ;; set window size
      (set-face-attribute 'default nil :family "courier new" :height 80)
      (setq my-window-width 222)
      (setq my-window-height 81)

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

	  (defun ajt-build-tags ()
		(interactive)
		(shell-command "c:& cd %HOME%/.emacs.d/ebrowse/& ruby makefiles.rb")
		(shell-command "c:& cd %HOME%/.emacs.d/etags/& ruby makefiles.rb"))

      (defun ajt-browse ()
		"open up ebrowser"
		(interactive)
		(find-file "~/.emacs.d/ebrowse/BROWSE"))

	  (defun ajt-dtp-search (arg)
		"search for a regex in all dtp files"
		(interactive "sgrep-regexp:")
		(start-process-shell-command "my-process" "*dtp-search*"
									 (concat "find d:/tras/cdc/dtp d:/tras/code/dtp ( -name \"*.dtp\" -o -name \"*.dtpinc\" ) -type f -print0 | xargs -0 -e grep -nH -e " arg))
		(pop-to-buffer "*dtp-search*")
		(compilation-mode))

	  (defun ajt-code-search (arg)
		"search for a regex in all code files"
		(interactive "sgrep-regexp:")
		(start-process-shell-command "my-process" "*code-search*"
									 (concat "find d:/tras/cdc/runtime d:/tras/code/game/ ( -name \"*.cpp\" -o -name \"*.h\" ) -type f -print0 | xargs -0 -e grep -nH -e " arg))
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

;; no highlighted text when selecting.
(transient-mark-mode nil)

;; no startup message
(setq inhibit-startup-message t)

;; Change backup behavior to save in a directory, not in a miscellany
;; of files all over the place.
(setq backup-by-copying t
	  backup-directory-alist '(("." . "~/.emacs.d/backups"))
	  delete-old-versions t
	  kept-new-versions 6
	  kept-old-versions 2
	  version-control t)

;; hide tool bar & menu
(menu-bar-mode -1)
(tool-bar-mode 0)

;; color-theme
(setq load-path (cons "~/.emacs.d/color-theme-6.6.0" load-path))
(require 'color-theme)
(color-theme-initialize)
(if window-system
	(color-theme-ajt-no-bold-blue-sea)
    (color-theme-charcoal-black))

;(color-theme-charcoal-black)
(color-theme-ajt-no-bold-blue-sea)
;(color-theme-high-contrast)

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

;; scrolling output
(setq compilation-scroll-output t)

;; ido find-file & buffer switching is awesome
(require 'ido)
(ido-mode t)

;; other-frame
(global-set-key [f5] 'other-frame)

;; prevent this from invoking suspend-frame, cause it's ANNOYING
(global-set-key "\C-x\C-z" nil)

;; glsl-mode
(load-library "glsl-mode")

;; ruby-mode NOTE: now included in 23.1
(when (< emacs-major-version 23)
  (load-library "ruby-mode"))

;; assign modes to file extentions
(setq auto-mode-alist
      (append '(("\\.cpp$" . c++-mode)
				("\\.h$" . c++-mode)
				("\\.c$" . c-mode)
				("\\.txt$" . text-mode)
			    ("\\.rb$" . ruby-mode)
				("\\.dd$" . ruby-mode)   ; bbq data definition file
				("\\.di$" . ruby-mode)   ; bbq data instance file
				("\\.bin$" . hexl-mode)  ; binary blob
				("\\.glsl$" . glsl-mode)
				("\\.m$" . objc-mode)
				("\\.mm$" . objc-mode)
				("\\.el$" . lisp-mode)
				("\\.dtp$" . xml-mode)
				("\\.dtpinc$" . xml-mode)
				("\\.go$" . go-mode)
				("\BROWSE$" . ebrowse-tree-mode)
			  auto-mode-alist)))

(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

;; irc chat
;;(require 'erc)

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
  "Load my init.el file into a buffer"
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
		