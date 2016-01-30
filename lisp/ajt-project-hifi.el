; WTF: sometimes I have to do this in order for some commands to work.
(setq flymake-mode nil)

(setq ajt-hifi-path "~/code/hifi")
;(when (and is-windows-machine (not (string= hostname "blackholesun")))
;  (setq ajt-hifi-path "C:/Users/Anthony/code/hifi"))

;; hifi javascript search with regex
(defun ajt-hifi-js-search (arg)
  "Search for a regex in all hifi javascript files"
  (interactive "shifi-js:")
  (let ((path (concat ajt-hifi-path "/examples/")))
    (ajt-grep-find arg (list path) '("*.js"))))

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
  (ajt-grep-find arg (list ajt-hifi-path "!*build/*" "!jquery-2.1.4.min.js") '("*.cc" "*.cpp" "*.h" "*.hpp" "*.txt")))

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

(defun ajt-quat-to-axis-angle (q)
  (let ((qx (car q))
        (qy (cadr q))
        (qz (caddr q))
        (qw (cadddr q)))
    (let ((angle (* 2.0 (acos qw)))
          (s (- 1.0 (* qw qw))))
      (if (<= s 0.0)
          (list 1.0 0.0 0.0 angle)
        (list (/ qx (sqrt s))
              (/ qy (sqrt s))
              (/ qz (sqrt s))
              angle)))))

(defun ajt-vec3-len (v)
  (let ((vx (car v)) (vy (cadr v)) (vz (caddr v)))
    (sqrt (+ (* vx vx) (* vy vy) (* vz vz)))))

(defun ajt-vec3-add (v1 v2)
  (let ((v1x (car v1)) (v1y (cadr v1)) (v1z (caddr v1))
        (v2x (car v2)) (v2y (cadr v2)) (v2z (caddr v2)))
    (list (+ v1x v2x) (+ v1y v2y) (+ v1z v2z))))

(defun ajt-vec3-sub (v1 v2)
  (let ((v1x (car v1)) (v1y (cadr v1)) (v1z (caddr v1))
        (v2x (car v2)) (v2y (cadr v2)) (v2z (caddr v2)))
    (list (- v1x v2x) (- v1y v2y) (- v1z v2z))))


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
    (find-file "/Users/anthony/AppData/Local/High Fidelity/Stack Manager/resources/models.json.gz"))
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

