(load "ajt-vector-math")

;; tribexr source code
(setq ajt-tribexr-path "C:/msys64/home/ajthy/code/vrdj-school")

;; unreal source code
(setq ajt-unreal-path "C:/msys64/home/ajthy/code/UnrealEngine/Engine/Source/Runtime")

;; mixreality source code
(setq ajt-mixreality-path "C:/msys64/home/ajthy/code/mixreality")

;; tribexr project cpp search with regex
(defun ajt-tribexr-cpp-search (arg)
  "Search for a regex in tribexr source cpp files"
  (interactive "stribexr-cpp:")
  (ajt-grep-find arg (list (concat ajt-tribexr-path "/Source")) '("*.h" "*.cpp")))

;; tribexr plugins cpp search with regex
(defun ajt-tribexr-plugins-cpp-search (arg)
  "Search for a regex in tribexr plugin cpp files"
  (interactive "stribexr-plugins-cpp:")
  (ajt-grep-find arg (list (concat ajt-tribexr-path "/Plugins") "!*/Build/*") '("*.h" "*.cpp")))

;; tribexr ini file search
(defun ajt-tribexr-ini-search (arg)
  "Search for a regex in tribexr ini files"
  (interactive "stribexr-ini:")
  (ajt-grep-find arg (list (concat ajt-tribexr-path "/Config")) '("*.ini")))

;; tribexr Build.cs file search
(defun ajt-tribexr-build-search (arg)
  "Search for a regex in tribexr ini files"
  (interactive "stribexr-build:")
  (ajt-grep-find arg (list ajt-tribexr-path) '("*.Build.cs" "*.uplugin" "*.uproject")))

;; tribexr nativized blueprint cpp search with regex
;; I needed this to track down a bug in the generated blueprint C++ code.
(defun ajt-tribexr-blueprint-cpp-search (arg)
  "Search for a regex in tribexr nativized blueprint cpp files"
  (interactive "stribexr-blueprint-cpp:")
  ; C:\msys64\home\ajthy\code\vrdj-school\Intermediate\Plugins\NativizedAssets\Windows\Game\Source\NativizedAssets\Private\BP_TribeTwitchManager__pf4024189247.cpp
  (ajt-grep-find arg (list  (concat ajt-tribexr-path "/Intermediate/Plugins/NativizedAssets/Windows/Game/Source/NativizedAssets")) '("*.h" "*.cpp")))

;; unreal engine cpp search with regex
(defun ajt-unreal-cpp-search (arg)
  "Search for a regex in all unreal engine cpp files"
  (interactive "sunreal-cpp:")
  (ajt-grep-find arg (list (concat ajt-unreal-path "")) '("*.h" "*.cpp")))

;; search mixreality python
(defun ajt-mixreality-py-search (arg)
  "Search for a regex in mixreality python files"
  (interactive "smixreality-py:")
  (ajt-grep-find arg (list ajt-mixreality-path) '("*.py")))

(defun ajt-mixreality-js-search (arg)
  "Search for a regex in mixreality js files"
  (interactive "smixreality-js:")
  (ajt-grep-find arg (list ajt-mixreality-path) '("*.js")))

(global-set-key [f9] 'ajt-tribexr-cpp-search)
(global-set-key [f10] 'ajt-mixreality-py-search)
;(global-set-key [f10] 'ajt-tribexr-plugins-cpp-search)
(global-set-key [f11] 'ajt-unreal-cpp-search)
;(global-set-key [f12] 'ajt-tribexr-ini-search)
;(global-set-key [f12] 'ajt-tribexr-blueprint-cpp-search)


;; andorid screencap via adb
;; arg is filename
(defun ajt-adb-screencap (arg)
  (interactive "sadb-screencap:")
  (let ((png-filename (concat ajt-tribexr-path "/" arg ".png")))
    (shell-command "adb shell \"screencap -p /sdcard/screen.png\"")
    (shell-command (concat "adb pull sdcard/screen.png " png-filename))
    (shell-command "adb shell \"rm /sdcard/screen.png\"")
    (find-file png-filename)))

;; android unreal console command
(defun ajt-adb-unreal-console-cmd (arg)
  (interactive "sunreal-console-cmd:")
  (shell-command (concat "adb shell \"am broadcast -a android.indent.action.RUN -e cmd '" arg "'\"")))

(defun ajt-adb-clean ()
  (interactive)
  (shell-command "adb uninstall com.TribeXR.TribeXR")
  (shell-command "adb shell \"rm -rf /sdcard/UE4Game/TribeXR\""))

;; convert from linear color to sRGB
(defun ajt-linear-to-srgb (x)
  (mapcar (lambda (x) (expt x (/ 1.0 2.2))) x))

;; open tribexr GameUserSettings.ini for production build.
(defun ajt-tribe-open-ini ()
  (interactive)
  (find-file "/Users/ajthy/AppData/Local/TribeXR/Saved/Config/WindowsNoEditor/GameUserSettings.ini"))

;; search the web for the term
(setq ajt-use-google nil)
(defun ajt-unreal-docs (arg)
  (interactive "sunreal-doc:")
  (if ajt-use-google
      (browse-url (concat "https://www.google.com/search?q=unreal+" (url-encode-url arg)))
    (browse-url (concat "https://duckduckgo.com/?q=!ducky+unreal+" (url-encode-url arg)))))


(setq path-to-ctags "~/code/UnrealEngine")


