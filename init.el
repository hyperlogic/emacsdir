
;; used to determine which system are we running on.
(setq uname (substring (shell-command-to-string "uname") 0 -1))
(setq hostname (substring (shell-command-to-string "hostname") 0 -1))

;; TODO: make this more robust
(setq is-windows-machine (or (string= uname "MINGW32_NT-6.1")
                             (string= uname "MSYS_NT-6.3")
                             (string= uname "MSYS_NT-10.0")))
(setq is-macintosh-machine (string= uname "Darwin"))

;; emacsclient can be used to edit files from a terminal
(if window-system
    (server-start))

;;
;; path & autoloads
;;

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/emacs-color-themes/themes")

(require 'cl)
(require 'thingatpt)
(require 'powerline)

;;
;; init melpa package manager
;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; some custom git functions
(load "ajt-git")

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
(autoload 'csharp-mode "csharp-mode")
(autoload 'thrift-mode "thrift-mode")

(add-to-list 'load-path "~/.emacs.d/lisp/lua-mode" t)
(autoload 'lua-mode "lua-mode")
(setq lua-indent-level 4)

(add-to-list 'load-path "~/.emacs.d/lisp/rust-mode" t)
(autoload 'rust-mode "rust-mode")

;; assign modes to file extentions
(setq auto-mode-alist
      (append '(("\\.cpp\\'" . c++-mode)
                ("\\.h\\'" . c++-mode)
                ("\\.hpp\\'" . c++-mode)
                ("\\.c\\'" . c-mode)
                ("\\.cs\\'" . csharp-mode)
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
                ("\\.js\\'" . js-mode)
                ("\\.m\\'" . objc-mode)
                ("\\.mm\\'" . objc-mode)
                ("\\.go\\'" . go-mode)
                ("\\.coffee\\'" . coffee-mode)
                ("BROWSE\\'" . ebrowse-tree-mode)
                ("\\.lisp\\'" . common-lisp-mode)
                ("\\.el\\'" . lisp-mode)
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

;; by default use spaces to indent.
(setq-default indent-tabs-mode nil)

;; line highlight
(add-hook 'find-file-hook
          (lambda ()
            ;; don't enable line highlight for specific buffer names
            (unless (or (string-match "*ansi-term*" (buffer-name))
                    (string-match "*shell*" (buffer-name))
                    (string-match "*ajt-grep*" (buffer-name)))
              (progn
                (hl-line-mode)
                (set-face-foreground 'highlight nil)
                (when (boundp 'hl-line-face)
                  (if (and window-system use-dark-theme)
                      (set-face-background hl-line-face "midnight blue")
                    (set-face-background hl-line-face "light gray")))))))


;; better scroll wheel behavior
(if window-system
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))))

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

;; no highlighted text when selecting.
(transient-mark-mode 0)

;; syntax highlighting for c++
(setq c-basic-offset 4)
(setq tab-width 4)
(setq default-tab-width 4)

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

; fix for obsolute GREP_OPTIONS spew
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
              ;; (setq ansi-term-color-vector [default "#000000" "#963F3C" "#2F9B25" "#9F9D25" "#0042cF" "#FF2180" "#279C9B" "#FFFFFF"])
              (setq show-trailing-whitespace nil)))


(add-hook 'compilation-mode-hook '(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'diff-mode-hook '(lambda () (setq show-trailing-whitespace nil)))

;;
;; term
;;

;; 't if buffer with name is open
;; nil otherwise
(defun ajt-buffer-open (name)
  (loop for b in (buffer-list) do
        (if (string-equal (buffer-name b) name)
            (return 't))))

(setq-default ajt-use-ansi-term (not is-windows-machine))
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


;;
;; color theme
;;

(setq use-dark-theme 't)

(when (and window-system use-dark-theme)
  (load-theme 'granger t)
  (when (boundp 'hl-line-face)
    (set-face-background hl-line-face "midnight blue")))

;;
;; platform specific stuff
;;

(if is-windows-machine
    (load "ajt-windows-init"))

(if is-macintosh-machine
    (load "ajt-macintosh-init"))


;;
;; project specific stuff
;;

(load "ajt-project-hifi")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("7030bc3b02bdc470fa1884522ffdb09466179aea1d9246335011ba7f9c2ccc24" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" "53c542b560d232436e14619d058f81434d6bbcdc42e00a4db53d2667d841702e" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "1989847d22966b1403bab8c674354b4a2adf6e03e0ffebe097a6bd8a32be1e19" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "0ebe0307942b6e159ab794f90a074935a18c3c688b526a2035d14db1214cf69c" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
