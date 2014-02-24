;; my not so little .emacs.el file

;; TODO: move into modules.
;; header-swap
;; grep
;; home-laptop config
;; ngmoco config
;; crystal dynamics config
;; special buffer stuff
;; smart-beginning of line

;; NOTE: occur is the command I always forget the name of. :)
;; NOTE: occur can take a optional context value!

;; which system are we running on.
(setq uname (substring (shell-command-to-string "uname") 0 -1))
(setq hostname (substring (shell-command-to-string "hostname") 0 -1))

;; add .emacs.d to load path
(setq load-path (cons "~/.emacs.d/" load-path))

;; show line & column in modeline
(setq column-number-mode t)

;; show filesize and % in modeline
(size-indication-mode 1)

;; show time in modeline
(display-time-mode 1)

;; display full path on modeline as well as filename.
(defun ajt-set-mode-line ()
  (interactive "*")
  (setq-default mode-line-buffer-identification
                '(#("%12f %b" 0 4 (local-map (keymap (header-line keymap (mouse-3 . mode-line-next-buffer) (down-mouse-3 . ignore) (mouse-1 . mode-line-previous-buffer) (down-mouse-1 . ignore)) (mode-line keymap (mouse-3 . mode-line-next-buffer) (mouse-1 . mode-line-previous-buffer))) mouse-face mode-line-highlight help-echo "Buffer name\nmouse-1: previous buffer\nmouse-3: next buffer" face mode-line-buffer-id)))))
(ajt-set-mode-line)

;; emacsclient can be used to edit files from a terminal
(server-start)

;; revert buffers when they change on disk (except if they are modified)
(setq revert-without-query '("."))
(global-auto-revert-mode)
(setq auto-revert-verbose nil) ;; stfu

;; default window size
(setq my-window-width 80)
(setq my-window-height 25)

;; Show trailing whitespace, except for term-mode and compilation-mode
(setq-default show-trailing-whitespace t)

 (add-hook 'term-mode-hook
           '(lambda ()
              ;; BROKEN in emacs 24.3 *sigh*
              ;; override ansi-colors                   black     red       green     yellow    blue      magenta   cyan      white
              ;; (setq ansi-term-color-vector [default "#000000" "#963F3C" "#2F9B25" "#9F9D25" "#0042cF" "#FF2180" "#279C9B" "#FFFFFF"])
              (setq show-trailing-whitespace nil)))

;(add-hook 'compilation-mode-hook '(lambda () (setq show-trailing-whitespace nil)))

(add-hook 'diff-mode-hook '(lambda () (setq show-trailing-whitespace nil)))

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

;; ruby-mode NOTE: now included in 23.1,
;; in 24.3 last-command-char not defined anymore?!?
(when (or (< emacs-major-version 23) (= emacs-major-version 24))
  (load-library "ruby-mode")
)

;; turn off line wrapping.
(set-default 'truncate-lines t)

;; by default use spaces to indent.
;; NOTE: overridden for ngmoco
(setq-default indent-tabs-mode nil)

;(add-hook 'diff-mode-hook '(lambda () (setq show-trailing-whitespace nil)))

; hl-line color
(set-face-background hl-line-face "gray18")

;; highlight curent line NOTE: better then hl-line mode
;(require 'highlight-current-line)
;(if window-system
;    (highlight-current-line-on t))

;; highlight-current-line-face
;(custom-set-faces
; '(highlight-current-line-face ((t (:background "gray90"))))
;)

;; turn off line highlighting on ansi-term
;;(setq highlight-current-line-ignore-regexp
;;      (concat highlight-current-line-ignore-regexp "\\|\\*ansi-term\\*"))

;;
;; Refresh the compilation-mode on the ajt-grep buffer
;;
(defun ajt-refresh-compilation-mode-on-grep (process event)
  (let ((prev-buffer (current-buffer)))
	(switch-to-buffer "*ajt-grep*")
	(end-of-buffer)
	(setq buffer-read-only nil)
	(insert (format "\nProcess %s %s\n" process event))
	(setq buffer-read-only 't)
	(compilation-mode)
	(switch-to-buffer prev-buffer)))

;;
;; Pops up a grep process in a buffer named *ajt-grep*
;;
(defun ajt-grep-find-shell-cmd (cmd)
  ;; echo the cmd line to the *Message* log
  (message cmd)

  ;; exec command in a new process
  (let ((process (start-process-shell-command "ajt-grep" "*ajt-grep*" cmd))
		(buffer (pop-to-buffer "*ajt-grep*")))
	(set-process-sentinel process 'ajt-refresh-compilation-mode-on-grep)
	(compilation-mode)))

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


;; invoke git blame on current buffer.
(defun ajt-git-blame ()
  (interactive)
  (let ((line (line-number-at-pos)))
    (shell-command (concat "git blame -w \"" (buffer-file-name) "\"") "*ajt-blame*")
    (pop-to-buffer "*ajt-blame*")
    (goto-line line)))

;; invoke git log on current buffer.
(defun ajt-git-log ()
  (interactive)
  (let ((line (line-number-at-pos)))
    (shell-command (concat "git log " (buffer-file-name)) "*ajt-log*")
    (pop-to-buffer "*ajt-log*")
    (goto-line line)))

;; invoke git diff and pipe result into a buffer.
(defun ajt-git-diff ()
  (interactive)
  (shell-command "git diff -w" "*ajt-git-diff*")
  (pop-to-buffer "*ajt-git-diff*")
  (diff-mode))


;; better scroll wheel behavior
(if window-system
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))))

;;
;; bluerose - arch home laptop
;;

(if (string= "bluerose" hostname)
    (progn
      (set-default-font "6x13")

      (defun ajt-make-server ()
        (interactive)
        (shell-command "cd ~/WebGame; make server&" "*ajt-make-server*")
        (pop-to-buffer "*ajt-make-server*"))

      (defun ajt-make-server-no-typecheck ()
        (interactive)
        (shell-command "cd ~/WebGame; make server typecheck=false&" "*ajt-make-server*")
        (pop-to-buffer "*ajt-make-server*"))

      ;; WebGame javascript search with regex
      (defun ajt-js-search (arg)
        "Search for a regex in all ngCore javascript files"
        (interactive "sngcore-js:")
        (ajt-grep-find arg '("~/WebGame" "!/Users/athibault/WebGame/Flash/*" "!*build/*" "!*Bootstrap/*") '("*.js" "!application.js")))

      ;; WebGame cpp search with regex
      (defun ajt-cpp-search (arg)
        "Search for a regex in all ngCore cpp files"
        (interactive "sngcore-cpp:")
        (ajt-grep-find arg '("~/WebGame" "!/Users/athibault/WebGame/android/jni/utils/v8/*" "!/Users/athibault/WebGame/NGGameTech/export") '("*.cc" "*.cpp" "*.h" "*.mm" "*.m")))

      ;; WebGame java search with regex
      (defun ajt-java-search (arg)
        "Search for a regex in all ngCore java files"
        (interactive "sngcore-java:")
        (ajt-grep-find arg '("~/WebGame/android") '("*.java")))

      (defun ajt-ngcore ()
        (interactive)
        (shell-command "cd ~/WebGame/Linux; ./ngCore&" "*ajt-ngcore*")
        (pop-to-buffer "*ajt-ngcore*"))

      (global-set-key [f8] 'ajt-js-search)
      (global-set-key [f9] 'ajt-cpp-search)
      (global-set-key [f10] 'ajt-java-search)
      (global-set-key [f11] 'ajt-ngcore)

      ; TODO: set up emacsclient as default EDITOR
))

;;
;; dodecahedron - home laptop
;;
(if (string= "Darwin" uname)
    (progn

      ;; go stuff
;      (add-to-list 'load-path "~/go/misc/emacs/" t)
;      (require 'go-mode-load)

      ;; turn off anti-aliasing
      ;(setq mac-allow-anti-aliasing nil)

      ;; turn on anti-aliasing
      ;(setq mac-allow-anti-aliasing t)

      ;; Note: be sure to also enter the following into the shell
      ;; defaults write org.gnu.Emacs AppleAntiAliasingThreshold 128

      ;; tiny xcode font
      ;(setq mac-allow-anti-aliasing nil)
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

      ;; use aspell
      (setq-default ispell-program-name "aspell")

      ;; show entire kill ring in a buffer
      (require 'browse-kill-ring)

      ;; home
      (if (or (string= "dodecahedron" hostname) (string= "dodecahedron.local" hostname))
          (progn
             (setenv "PATH" "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/Users/ajt/bin")
             (setq-default ispell-program-name "/usr/local/bin/aspell")

             ;; I need this now for some reason to make revert-buffer work! wtf
             (flymake-mode nil)

            ))

      ;; work @ ngmoco:)
      (if (or (string= "anthony-thibault_macbook-pro.local" (downcase hostname))
              (string-match ".*corp\.ngmoco\.com$" (downcase hostname))
              (string-match "sfo.500405.mac" (downcase hostname)))
          (progn

            ;(require 'magit)
            ;(setq magit-repo-dirs nil)

            ; nice small font
            ;(set-face-attribute 'default nil :family "Menlo" :height 105 :weight 'normal)
            ;(set-face-attribute 'default nil :family "Inconsolata" :height 120 :weight 'bold)
            ;(set-face-attribute 'default nil :family "Droid Sans Mono" :height 110)
            ;(set-face-attribute 'default nil :family "Ubuntu Mono" :height 130 :weight 'normal)
            (set-face-attribute 'default nil :family "Monaco" :height 100 :weight 'normal)
            ;(set-face-attribute 'default nil :family "Courier Prime" :weight 'normal :height 100)

            ; tiny xcode font
            ;(setq mac-allow-anti-aliasing nil)
            ;(set-face-attribute 'default nil :family "Monaco" :height 100 :weight 'normal)
            ;(set-face-attribute 'default nil :family "Monaco" :height 140 :weight 'normal :slant 'normal)
            ;(set-face-attribute 'default nil :family "PragmataPro" :height 95 :weight 'normal :slant 'normal)
            ;(set-face-attribute 'default nil :family "Courier Prime" :weight 'normal :slant 'italic :height 110)

            (setq mac-allow-anti-aliasing 't)
            (set-face-attribute 'default nil :family "Monaco" :height 120)

            ;(set-face-attribute 'default nil :family "PragmataPro" :height 105)

            (setenv "PATH" "/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:~/bin:~/WebGame/Tools:~/WebGame/Resources:~/WebGame/android/DevKits/sdk/platform-tools:~/.emacs.d")

            ; omg dec vt220
            ;(set-face-attribute 'default nil :family "Glass TTY VT220" :height 170)

            ;; Only useful when screen is maximized, and only works on a patched emacs (from Homebrew)
            ;(ns-toggle-fullscreen)

            ;; ngCore uses tabs...
            (setq-default indent-tabs-mode 't)

			;; every file open goes through here!
            (add-hook 'find-file-hook (lambda ()

										;; line highlighting
										(unless (or (string-match "*ansi-term*" (buffer-name))
												(string-match "*ajt-grep*" (buffer-name)))
										  (hl-line-mode))

										; use spaces for NGGo and my personal projects
										(if (or (string-match ".*/dev/.*" (buffer-file-name))
												(string-match ".*/code/.*" (buffer-file-name)))
											(setq indent-tabs-mode nil))
										; MonoDevelop has trailing whitespace everywhere!
										(if (or (string-match ".*/unity/TF2/.*" (buffer-file-name)))
											(setq show-trailing-whitespace nil))))

            ;; this file should be spaces
            (setq indent-tabs-mode nil)

            (defun dired-sort-size ()
              "Dired sort by size."
              (interactive)
              (dired-sort-other (concat dired-listing-switches "S")))

            (defun dired-sort-name ()
              "Dired sort by name."
              (interactive)
              (dired-sort-other (concat dired-listing-switches "")))

            ;;(require 'ajt-js-flymake)
            ;;(setq flymake-log-level 3)
            ;;(add-hook 'javascript-mode-hook
            ;;          (lambda () (flymake-mode t)))

			;; I need this now for some reason to make revert-buffer work, on js files wtf.
			(flymake-mode nil)

            ;; use aspell
            (setq-default ispell-program-name "/usr/local/bin/aspell")

            ;; older branches still use android sdkr12
            (setq use-sdkr12 nil)
            (if use-sdkr12
                (setenv "PATH" (concat (getenv "PATH") ":/Users/athibault/WebGame/android/DevKits/sdkr12/platform-tools")))

            (setenv "PATH" (concat (getenv "PATH") ":~/WebGame/submodules/coreTools/android-ndk-r6b:~/bin"))

            ;; WebGame javascript search with regex
            (defun ajt-js-search (arg)
              "Search for a regex in all ngCore javascript files"
              (interactive "sngcore-js:")
              (ajt-grep-find arg '("~/WebGame" "!/Users/athibault/WebGame/Flash/*" "!*build/*" "!*Bootstrap/*" "!*v8/*" "!*SpiderMonkey/*" "!*docs/*" "!*node-jsdoc-toolkit/*") '("*.js" "!application.js")))

            ;; WebGame cpp search with regex
            (defun ajt-cpp-search (arg)
              "Search for a regex in all ngCore cpp files"
              (interactive "sngcore-cpp:")
              (ajt-grep-find arg '("~/WebGame" "!/Users/athibault/WebGame/android/jni/utils/v8/*" "!/Users/athibault/WebGame/NGGameTech/export") '("*.cc" "*.cpp" "*.h" "*.mm" "*.m")))

            ;; WebGame java search with regex
            (defun ajt-java-search (arg)
              "Search for a regex in all ngCore java files"
              (interactive "sngcore-java:")
              (ajt-grep-find arg '("~/WebGame/android") '("*.java")))

            ;; NGGo js search with regex
            (defun ajt-go-search (arg)
              "Search for a regex in all ngGO js files"
              (interactive "snggo-js:")
              (ajt-grep-find arg '("~/dev/ngGo/NGGo" "!*build/*") '("*.js" "!application.js")))

            ;; transformers js search with regex
            (defun ajt-trans-search (arg)
              "Search for a regex in all js files in transformers game code"
              (interactive "stransformers-js:")
              (ajt-grep-find arg '("~/dev/transformers/Client/Transformers/Code" "!*build/*") '("*.js" "!application.js")))

			(defun ajt-tf2-search (arg)
			  "Search for a regex in all c# files in tf2 game code"
			  (interactive "stf2-c#:")
			  (ajt-grep-find arg '("~/unity/TF2/Assets") '("*.cs")))

			(defun ajt-st-search (arg)
			  "Search for a regex in all c# files in tanuki/sharedTec game code"
			  (interactive "stf2-c#:")
			  (ajt-grep-find arg '("~/unity/sharedTech/client/Assets") '("*.cs")))

            ;; puzzles js search with regex
            (defun ajt-puzz-search (arg)
              "Search for a regex in all js files in puzzles game code"
              (interactive "spuzz-js:")
              (ajt-grep-find arg '("~/dev/puzzles/Client/Puzzles/Code" "!*build/*") '("*.js" "!application.js")))


			(defun ajt-narwhal-search (arg)
			  "Search narwhal javascript"
			  (interactive "snarwhal-js:")
			  (ajt-grep-find arg '("~/OtherWebGame/NGCore/Client/Social/_Internal/US/Interface/Public" "!*build/*") '("*.js" "!application.js")))

			(defun ajt-mobage-search (arg)
			  "Search ndk mobage javascript"
			  (interactive "smobage-js:")
			  (ajt-grep-find arg '("~/WebGame/submodules/ndk-dist/mobage-interface-ndk-ngcore/WW" "!*build/*") '("*.js" "!application.js")))

            ;; remove .ngmoco directory on android device.
            (defun ajt-anuke ()
              (interactive)
              (shell-command "adb shell rm -r /mnt/sdcard/.ngmoco" nil)
              (shell-command "adb shell rm -r /storage/emulated/0/Android/data/com.mobage.ww/cache/.ngmoco/" nil)
              (shell-command "adb shell rm -r /mnt/sdcard/Android/data/com.mobage.ww/cache/.ngmoco" nil)
              )

            ;; run "make server" in an emacs buffer
            (defun ajt-make-server ()
              (interactive)
              (shell-command "cd ~/WebGame; make server&" "*ajt-make-server*")
              (pop-to-buffer "*ajt-make-server*"))

            (defun ajt-make-server-no-typecheck ()
              (interactive)
              (shell-command "cd ~/WebGame; make server typecheck=false&" "*ajt-make-server*")
              (pop-to-buffer "*ajt-make-server*"))

            ;; launch gamejs
            (defun ajt-arun ()
              (interactive)
              (save-some-buffers)
              (shell-command "adb shell am broadcast -a com.ngmoco.gamejs.STOP > /dev/null" nil)
              (shell-command "sleep 1" nil)
              (shell-command "adb shell am start -a com.ngmoco.gamejs.RUN -e nativeLog true > /dev/null" nil)
              (pop-to-buffer "*adb-logcat*")
              (adb-clear))

            (defun ajt-astop ()
              (interactive)
              (shell-command "adb shell am broadcast -a com.ngmoco.gamejs.STOP > /dev/null" nil))

            (defun ajt-arun-game (game)
              (interactive "sgame:")
              (save-some-buffers)
              (shell-command "adb shell am broadcast -a com.ngmoco.gamejs.STOP > /dev/null" nil)
              (shell-command "sleep 1" nil)
              (shell-command (concat "adb shell am start -a com.ngmoco.gamejs.RUN -e nativeLog true -e jsLog true -e game " game " > /dev/null") nil)
              (pop-to-buffer "*adb-logcat*"))

            (defun ajt-ww-astop ()
              (interactive)
              (shell-command "adb shell am broadcast -a com.ngmoco.gamejs.STOP > /dev/null" nil))

            (defun ajt-ww-arun ()
              (interactive)
              (save-some-buffers)
              (shell-command "adb shell am broadcast -a com.mobage.ww.STOP > /dev/null" nil)
              (shell-command "sleep 1" nil)
              (shell-command "adb shell am start -a com.mobage.ww.RUN -e nativeLog true > /dev/null" nil)
              (pop-to-buffer "*adb-logcat*")
              (adb-clear))

			; for ios logs
			; tail -f ~/Library/Logs/iOS\ Simulator/7.0.3-64/system.log
            (defun adb-logcat ()
              (interactive)
              (start-process "*adb-logcat*" "*adb-logcat*" "/bin/sh" "-c" "adb logcat -v threadtime")
              (pop-to-buffer "*adb-logcat*")
              (buffer-disable-undo))

            (defun adb-clear ()
              (interactive)
              (pop-to-buffer "*adb-logcat*")
              (delete-region (point-min) (point-max)))

            (defun ajt-ngboot ()
              (interactive)
              (save-some-buffers)
              (if (get-buffer "*ajt-ngboot*") (kill-buffer (get-buffer "*ajt-ngboot*")))
              (message "stopping game")
              (shell-command "adb shell am broadcast -a com.ngmoco.gamejs.STOP > /dev/null" nil)
              (message "building ngboot...")
              (if (not (equal 0 (shell-command "cd ~/WebGame/NGBoot/; make jake" "*ajt-ngboot*")))
                  (progn
                    (pop-to-buffer "*ajt-ngboot*")
                    (compilation-mode)
                    (error "ngboot build failed"))
                (kill-buffer "*ajt-ngboot*"))
              (message "starting game")
              (shell-command "adb shell am start -a com.ngmoco.gamejs.RUN -e nativeLog true > /dev/null" nil)
              (pop-to-buffer "*adb-logcat*")
              )

            ;; (defun ajt-narwhal ()
            ;;   (interactive)
            ;;   (save-some-buffers)
            ;;   (if (get-buffer "*ajt-narwhal*") (kill-buffer (get-buffer "*ajt-narwhal*")))
            ;;   (message "stopping game")
            ;;   (shell-command "adb shell am broadcast -a com.ngmoco.gamejs.STOP > /dev/null" nil)
            ;;   (message "building narwhal...")
            ;;   (if (not (equal 0 (shell-command "cd ~/WebGame/submodules/narwhal/Bundles/MobageBoot/; make" "*ajt-narwhal*")))
            ;;       (progn
            ;;         (pop-to-buffer "*ajt-narwhal*")
            ;;         (compilation-mode)
            ;;         (error "narwhal build failed"))
            ;;     (kill-buffer "*ajt-narwhal*"))
            ;;   (message "starting game")
            ;;   (shell-command "adb shell am start -a com.ngmoco.gamejs.RUN -e nativeLog true > /dev/null" nil)
            ;;   (pop-to-buffer "*adb-logcat*")
            ;;   ;(ajt-logcat-mode)
            ;;   )

            ;; key bindings
            ;(global-set-key [f8] 'ajt-js-search)
            (global-set-key [f8] 'ajt-trans-search)
            (global-set-key [f9] 'ajt-cpp-search)
            (global-set-key [f10] 'ajt-java-search)
            (global-set-key [f11] 'ajt-arun)
            (global-set-key [C-f11] 'ajt-arun-game)

            ;; bake, build c++ code and install on device.
            ;; export JENKINS=TRUE
            (setq compile-command "cd ~/WebGame/; make afast")

            ;; build c++ code only for android.
            ;(setq compile-command "cd ~/WebGame/android/; ./DevKits/ndkr6/ndk-build")

            ;; ios command line build
            ;(setq compile-command "cd ~/WebGame/; xcodebuild -project webgame.xcodeproj -alltargets -configuration Debug")

            ;; TO build c++ code only do this:

            ;; make tags
            (defun ajt-make-tags ()
              "Make TAGS"
              (interactive)
              (ajt-grep-find-shell-cmd "~/WebGame/samples/ajt/bin/etagsGen.rb ~/WebGame/TAGS ~/WebGame \!~/WebGame/android/jni/utils/v8 \!~/WebGame/NGGameTech/export"))

            ;; use TAGS file in these dirs.
            (setq tags-table-list '("~/WebGame"))

            ;; mark "application.js" as read only.
            (defun ajt-application-js-as-read-only ()
              (if (string-equal (buffer-name (current-buffer)) "application.js")
                  (setq buffer-read-only t)))
            (add-hook 'find-file-hook 'ajt-application-js-as-read-only)

            ;; ndk-gdb --launch=com.ngmoco.gamejs.activity.GameJSActivity

            ;; it was cute, for 5 seconds.
            ;(setq load-path (cons "~/.emacs.d/nyan-mode" load-path))
            ;(load-library "nyan-mode")
            ;(nyan-mode)

            ; better percent indicator in modeline
            ;(load-library "sml-modeline")
            ;(sml-modeline-mode)

            (setq load-path (cons "~/.emacs.d/coffee-mode" load-path))
            (load-library "coffee-mode")

            (defun coffee-custom ()
              "coffee-mode-hook"
              (set (make-local-variable 'tab-width) 2))

            ; ruby tab depth
            ; (setq ruby-indent-level 4))

            (add-hook 'coffee-mode-hook
                      '(lambda() (coffee-custom)))

            ))
      ))
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

;; enable scroll bars
(if window-system
    (scroll-bar-mode 1))

;; no gutters
(fringe-mode '(0 . 0))

; color-theme
;(setq load-path (cons "~/.emacs.d/color-theme-6.6.0" load-path))
;(require 'color-theme)

;(add-to-list 'load-path "~/.emacs.d/zenburn-emacs" t)
;(require 'color-theme-zenburn)
;(color-theme-initialize)

;; ;; ;; try out solarized
;; (add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized" t)
;; (require 'color-theme-solarized)
;; (setq solarized-bold nil)
;; (setq solarized-italic nil)
;; (setq solarized-broken-srgb t)
;; (setq solarized-contrast 'high)

;; (if window-system
;;  (color-theme-solarized-light))

;; (load "ajt-color-themes.el")
;; (color-theme-ajt-no-bold-blue-sea)
;; (if window-system
;;     (color-theme-zenburn))

(when (and (>= emacs-major-version 24) window-system)
  (load-theme 'deeper-blue))

;; syntax highlighting for c++
(setq c-basic-offset 4)
(setq tab-width 4)
(setq default-tab-width 4)

;; for linuxy c code
;;(setq tab-width 8)
;;(setq default-tab-width 8)

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
(setq ido-enable-flex-matching t)
(setq completion-ignored-extensions (cons ".d" completion-ignored-extensions))

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

;; rust-mode
(add-to-list 'load-path "~/.emacs.d/rust-mode" t)
(require 'rust-mode)

;; jsx-mode
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

(load "typescript/TypeScript")

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
                ("\\.jsfl\\'" . js-mode) ; flash javascript plug-in
                ("\\.m\\'" . objc-mode)
                ("\\.mm\\'" . objc-mode)
                ("\\.dtp\\'" . xml-mode)
                ("\\.dtpinc\\'" . xml-mode)
                ("\\.go\\'" . go-mode)
                ("\\.coffee\\'" . coffee-mode)
                ("BROWSE\\'" . ebrowse-tree-mode)
                ("\\.lisp\\'" . common-lisp-mode)
                ("\\.json\\'" . js-mode)
                ("\\.jake\\'" . js-mode)
                ("[jJ]akefile" . js-mode)
                (".boot[Cc]onfig" . js-mode)
                ("\\.rs\\'" . rust-mode)
                ("COMMIT_EDITMSG" . flyspell-mode)
                ("\\.jsx\\'" . jsx-mode)
				("\\.ts\\'" . typescript-mode)
                )
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
(transient-mark-mode 0)

;; slime for Clozure Lisp
;(set-language-environment "utf-8")
;(add-to-list 'load-path "~/install/slime-2010-07-01/")
;(setq inferior-lisp-program "/opt/local/bin/ccl64 -K utf-8")
;(require 'slime)
;(setq slime-net-coding-system 'utf-8-unix)
;(slime-setup '(slime-fancy))


(load-library "csharp-mode")
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; forth mode
(load "gforth.el")
(add-to-list 'auto-mode-alist '("\\.fs$" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.fth$" . forth-mode))
(add-to-list 'auto-mode-alist '("\\.f$" . forth-mode))

;; markdown mode
(load-library "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(setq load-path (cons "~/.emacs.d/emacs-jabber" load-path))
(load-library "jabber")

(defun ajt-jabber ()
  "Connect to ngmoco jabber server"
  (interactive)
  (jabber-connect "athibault" "jabber.ngmoco.com" nil nil "xxx.xxx" "jabber.ngmoco.com" 5222))

(defun ajt-jabber-conference ()
  "join the ngmoco ngcore multi-user channel conference"
  (interactive)
  (jabber-muc-join (jabber-read-account) "ngcore@conference.jabber.ngmoco.com" "athibault" 't))

