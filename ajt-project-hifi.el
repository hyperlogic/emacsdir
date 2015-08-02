(setq ajt-hifi-path "~/code/hifi")
(if is-windows-machine
      (setq ajt-hifi-path "C:/Users/Anthony/code/hifi"))

;; hifi javascript search with regex
(defun ajt-hifi-js-search (arg)
  "Search for a regex in all hifi javascript files"
  (interactive "shifi-js:")
  (let ((path (concat ajt-hifi-path "/examples")))
    (ajt-grep-find arg (list path) '("*.js"))))

;; WebGame cpp search with regex
(defun ajt-hifi-cpp-search (arg)
  "Search for a regex in all hifi cpp files"
  (interactive "shifi-cpp:")
  (ajt-grep-find arg (list ajt-hifi-path "!*build/*") '("*.cc" "*.cpp" "*.h" "*.hpp")))

(global-set-key [f8] 'ajt-hifi-js-search)
(global-set-key [f9] 'ajt-hifi-cpp-search)

(setq compile-command (concat "cd ~/code/hifi/build; xcodebuild -project hifi.xcodeproj/ -scheme interface -configuration Debug"))

(defun ajt-run-hifi ()
  (interactive)
  (shell-command "open ~/code/hifi/build/interface/Debug/interface.app"))

(global-set-key [f10] 'ajt-run-hifi)

;; make tags
(defun ajt-make-tags ()
  "Make TAGS"
  (interactive)
  (ajt-grep-find-shell-cmd "ruby ~/.emacs.d/etags/makefiles.rb"))


(setq tags-table-list '("~/.emacs.d/etags"))

