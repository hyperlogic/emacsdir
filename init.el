
;; used to determine which system are we running on.
(setq uname (substring (shell-command-to-string "uname") 0 -1))
(setq hostname (substring (shell-command-to-string "hostname") 0 -1))
(setq username (substring (shell-command-to-string "whoami") 0 -1))

;; TODO: make this more robust
(setq is-windows-machine (or (string= uname "MINGW32_NT-6.1")
                             (string= uname "MSYS_NT-6.3")
                             (string= uname "MSYS_NT-10.0")
                             (string= uname "MINGW64_NT-10.0")))
(setq is-macintosh-machine (string= uname "Darwin"))

;; emacsclient can be used to edit files from a terminal
(if window-system
    (server-start))

;; Try saving open buffers
(if window-system
    (desktop-save-mode 1))

;;
;; path & autoloads
;;

(add-to-list 'load-path "~/.emacs.d/lisp")

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
                ("\\.cc\\'" . c++-mode)
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

;;
;; detects if code buffer uses spaces or tabs for indenting
;; and preseves that setting
;;
(load "ajt-indent")

;;
;; color theme
;;

(setq use-dark-theme nil)

;; build in emacs themes

;; light themes
;;(load-theme 'adwaita)
;;(load-theme 'dichromacy)
;;(load-theme 'light-blue)
;;(load-theme 'tango)
;;(load-theme 'tsdh-light)
;;(load-theme 'whiteboard)

;; dark themes
;;(load-theme 'deeper-blue)
;;(load-theme 'manoj-dark)
;;(load-theme 'misterioso)
;;(load-theme 'tango-dark)
;;(load-theme 'tsdh-dark)
;;(load-theme 'wheatgrass)
;;(load-theme 'wombat)

(if (and window-system use-dark-theme)
    (progn
      (setq ajt-line-color "black")
      (load-theme 'granger)
      (when (boundp 'hl-line-face)
        (set-face-background hl-line-face ajt-line-color)))
  (progn
    (setq ajt-line-color "light gray")
    (load-theme 'tango)))

(add-hook 'find-file-hook
          (lambda ()
            (ajt-detect-and-set-indentation)
            ;; don't enable line highlight for specific buffer names
            (unless (or (string-match "*ansi-term*" (buffer-name))
                    (string-match "*shell*" (buffer-name))
                    (string-match "*ajt-grep*" (buffer-name)))
              (progn
                (if window-system
                    (hl-line-mode))
                (set-face-foreground 'highlight nil)
                (when (boundp 'hl-line-face)
                  (set-face-background hl-line-face ajt-line-color))))))

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
;; ajt-eslint & ajt-jsonlint
;;

(load "ajt-js-helpers")

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

(load "ajt-project-tribexr")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(beacon-color "#cc6666")
 '(custom-safe-themes
   '("72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "c9415c9f5a5ed67914d1d64a0ea7d743ef93516f1f2c8501bc5ffb87af2066d3" "7236acec527d58086ad2f1be6a904facc9ca8bf81ed1c19098012d596201b3f1" "8390138cba2f6ea28cd1dd64194f821d1d333baf3642fa097624dbebdca40ced" "a4395e069de3314001de4e139d6a3b1a83dcf9c3fdc68ee7b13eef6c2cba4ae3" "98db748f133d9bb82adf38f8ae7834eefa9eefd6f7ea30909213164e1aa36df6" "8f54cfa3f010d83d782fbcdc3a34cdc9dfe23c8515d87ba22d410c033160ad7e" "6f895d86fb25fac5dd4fcce3aec0fe1d88cf3b3677db18a9607cf7a3ef474f02" "7e5d400035eea68343be6830f3de7b8ce5e75f7ac7b8337b5df492d023ee8483" "b9e406b52f60a61c969f203958f406fed50b5db5ac16c127b86bbddd9d8444f7" "d9a28a009cda74d1d53b1fbd050f31af7a1a105aa2d53738e9aa2515908cac4c" "73320ccc14ab4987fe2e97cfd810b33a1f4a115f5f056c482c3d38a4429e1705" "78c01e1b7f3dc9e47bdd48f74dc98dc1a345c291f83b68ac8a1b40191f24d658" "620b9018d9504f79344c8ef3983ea4e83d209b46920f425240889d582be35881" "0c6a36393d5782839b88e4bf932f20155cb4321242ce75dc587b4f564cb63d90" "d548ac4bb4c8c0ba8f22476f5afcea11b7f1754065eefb118e1324f8a74883fb" "5642b25b6df4d6b63787cbc3d3ef07ca4cb7b0a7a00740ce8e9867c00e57632f" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "e27556a94bd02099248b888555a6458d897e8a7919fd64278d1f1e8784448941" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "fd22c8c803f2dac71db953b93df6560b6b058cb931ac12f688def67f08c10640" "56d10d2b60685d112dd54f4ba68a173c102eacc2a6048d417998249085383da1" "ff3c57a5049010a76de8949ddb629d29e2ced42b06098e046def291989a4104a" "0685ffa6c9f1324721659a9cd5a8931f4bb64efae9ce43a3dba3801e9412b4d8" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "0e2a7e1e632dd38a8e0227d2227cb8849f877dd878afb8219cb6bcdd02068a52" "7546a14373f1f2da6896830e7a73674ef274b3da313f8a2c4a79842e8a93953e" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "95d0ed21bb0e919be7687a25ad59a1c2c8df78cbe98c9e369d44e65bfd65b167" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" "e3c64e88fec56f86b49dcdc5a831e96782baf14b09397d4057156b17062a8848" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "ea066684e9ace1e618719fab683b24a0fbcd3de82692190b1fe54e6b1b2a29bc" "524cdbd8402dd9c5d03612b84a58eaa513656595535dffde428786802885b8d8" "81eedac030249f826681ab768953f5b0f671d4b069bd6c9c486f762644b24d7c" "ec2c86933a6e0b96f68f71d4b39ebdd67b43b0b32091b7689acb9acdc2a3e03b" "8426618fcc55f670f45b04f146933ae23caa1faad603a380e7a348660fd225ab" "28cf1f7cc54ab4ee1ba4a4644046bd661941be92ef8327af56909f340cb9d3d5" "6b234feec8db588ad5ec2a9d9d7b935f7a155104b25ccfb94d921c45a2ff7d22" default))
 '(fci-rule-color "#37474F")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#237AD3"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#579C4C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#777778"))
 '(linum-format " %7i ")
 '(objed-cursor-color "#D16969")
 '(package-selected-packages
   '(csv csv-mode kaolin-themes flucui-themes doom-themes color-theme-sanityinc-tomorrow almost-mono-themes use-package ewal sublime-themes))
 '(pdf-view-midnight-colors (cons "#d4d4d4" "#1e1e1e"))
 '(pos-tip-background-color "#16211C")
 '(pos-tip-foreground-color "#dcded9")
 '(rustic-ansi-faces
   ["#1e1e1e" "#D16969" "#579C4C" "#D7BA7D" "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(vc-annotate-background "#1e1e1e")
 '(vc-annotate-color-map
   (list
    (cons 20 "#579C4C")
    (cons 40 "#81a65c")
    (cons 60 "#acb06c")
    (cons 80 "#D7BA7D")
    (cons 100 "#d8ab79")
    (cons 120 "#d99c76")
    (cons 140 "#DB8E73")
    (cons 160 "#d38b8c")
    (cons 180 "#cc88a6")
    (cons 200 "#C586C0")
    (cons 220 "#c97ca3")
    (cons 240 "#cd7286")
    (cons 260 "#D16969")
    (cons 280 "#ba6c6c")
    (cons 300 "#a37070")
    (cons 320 "#8d7374")
    (cons 340 "#37474F")
    (cons 360 "#37474F")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
