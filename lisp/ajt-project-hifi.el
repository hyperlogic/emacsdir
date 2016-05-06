(load "ajt-vector-math")

;; WTF: sometimes I have to do this in order for some commands to work.
(setq flymake-mode nil)

(setq ajt-hifi-path "~/code/hifi")
;(when (and is-windows-machine (not (string= hostname "blackholesun")))
;  (setq ajt-hifi-path "C:/Users/Anthony/code/hifi"))

;; hifi javascript search with regex
(defun ajt-hifi-js-search (arg)
  "Search for a regex in all hifi javascript files"
  (interactive "shifi-js:")
  (let ((script-path (concat ajt-hifi-path "/scripts"))
        (archive-path (concat ajt-hifi-path "/script-archive")))
    (ajt-grep-find arg (list script-path archive-path "!*.min.js") '("*.js"))))

;; hifi cmake search with regex
(defun ajt-hifi-cmake-search (arg)
  "Search for a regex in all hifi cmake files"
  (interactive "shifi-cmake:")
  (let ((path (concat ajt-hifi-path)))
    (ajt-grep-find arg (list path "!*build/*") '("*.txt" "*.cmake" "*.txt"))))

;; cpp search with regex
(defun ajt-hifi-cpp-search (arg)
  "Search for a regex in all hifi cpp files"
  (interactive "shifi-cpp:")
  (ajt-grep-find arg (list ajt-hifi-path "!*build/*") '("*.cc" "*.cpp" "*.h" "*.hpp" "*.txt")))

(global-set-key [f7] 'ajt-hifi-cmake-search)
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

; tail -f /Users/anthony/AppData/Local/High\ Fidelity/Interface/Logs/hifi-log.txt
(defun ajt-hifi-log ()
  (interactive)
  (when is-windows-machine
    (find-file "/Users/anthony/AppData/Local/High Fidelity/Interface/Logs/hifi-log.txt"))
  (when is-macintosh-machine
    (find-file "~/Library/Application Support/High Fidelity/Interface/Logs/hifi-log.txt"))
  (auto-revert-tail-mode)
  (buffer-disable-undo))

(defun ajt-hifi-entity-server-log ()
  (interactive)
  (find-file "~/code/server-logs/entity-server.txt")
  (auto-revert-tail-mode)
  (buffer-disable-undo))

(defun ajt-hifi-ini ()
  "Load the Hifi fidelity ini file"
  (interactive)
  (when is-windows-machine
    (find-file "c:/Users/anthony/AppData/Roaming/High Fidelity/Interface.ini"))
  (when is-macintosh-machine
    (find-file "~/.config/highfidelity.io/Interface.ini")))

(defun ajt-hifi-server-json ()
  "Load the Hifi fidelity ini file"
  (interactive)
  (when is-windows-machine
    (find-file "/Users/anthony/AppData/Roaming/High Fidelity - dev/assignment-client/entities/models.json.gz"))
  (when is-macintosh-machine
    (find-file "~/Library/Application Support/High Fidelity/Stack Manager/resources/models.json.gz")))

(defun ajt-load-common-files ()
  "Load common files"
  (interactive)
  (let ((code-path "~/code/hifi")
        (docs-path "~/docs"))
    (find-file (concat code-path "/interface/src/Application.cpp"))
    (find-file (concat code-path "/interface/src/avatar/MyAvatar.cpp"))
    (find-file (concat code-path "/interface/src/avatar/SkeletonModel.cpp"))
    (find-file (concat code-path "/interface/src/avatar/Avatar.cpp"))
    (find-file (concat code-path "/interface/resources/meshes/defaultAvatar_full/avatar-animation.json"))
    (find-file (concat code-path "/libraries/animation/src/AnimSkeleton.cpp"))
    (find-file (concat code-path "/libraries/animation/src/Rig.cpp"))
    (find-file (concat code-path "/libraries/fbx/src/FBXReader.cpp"))
    (find-file (concat code-path "/libraries/render-utils/src/Model.cpp"))
    (find-file (concat code-path "/libraries/avatars/src/AvatarData.cpp"))
    (find-file (concat docs-path "/hifi/todo.md"))))

