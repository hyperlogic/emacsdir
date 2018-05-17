(load "ajt-vector-math")

;; WTF: sometimes I have to do this in order for some commands to work.
(setq flymake-mode nil)

(setq ajt-hifi-path "~/code/hifi")
(when (and is-windows-machine (string= hostname "blackholesun"))
  (setq ajt-hifi-path "e:/code/hifi"))

;; hifi javascript search with regex
(defun ajt-hifi-js-search (arg)
  "Search for a regex in all hifi javascript files"
  (interactive "shifi-js:")
  (let ((script-path (concat ajt-hifi-path "/scripts"))
        (archive-path (concat ajt-hifi-path "/script-archive"))
        (unpublished-path (concat ajt-hifi-path "/unpublishedScripts")))
    (ajt-grep-find arg (list script-path archive-path unpublished-path "!*.min.js") '("*.js"))))

;; qml search with regex
(defun ajt-hifi-qml-search (arg)
  "Search for a regex in all hifi javascript files"
  (interactive "shifi-qml:")
  (let ((qml-path (concat ajt-hifi-path "/interface/resources/qml")))
    (ajt-grep-find arg (list qml-path) '("*.qml"))))

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

;; shader search with regex
(defun ajt-hifi-shader-search (arg)
  "Search for a regex in all hifi cpp files"
  (interactive "shifi-shader:")
  (ajt-grep-find arg (list ajt-hifi-path "!*build/*") '("*.slv" "*.slf" "*.slg" "*.slh")))

(global-set-key [f7] 'ajt-hifi-shader-search)
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

; ini was recently changed to a json file...
(defun ajt-hifi-ini ()
  "Load the Hifi fidelity interface json file"
  (interactive)
  (when is-windows-machine
    (find-file "c:/Users/anthony/AppData/Roaming/High Fidelity - dev/Interface.json"))
  (when is-macintosh-machine
    (find-file "~/.config/highfidelity.io/Interface.json")))

(defun ajt-hifi-server-dev-json ()
  "Load the Hifi fidelity ini file"
  (interactive)
  (when is-windows-machine
    (find-file "/Users/anthony/AppData/Roaming/High Fidelity - dev/assignment-client/entities/models.json.gz"))
  (when is-macintosh-machine
    (find-file "~/Library/Application Support/High Fidelity/Stack Manager/resources/models.json.gz")))

(defun ajt-hifi-server-json ()
  "Load the Hifi fidelity ini file"
  (interactive)
  (when is-windows-machine
    (find-file "/Users/anthony/AppData/Roaming/High Fidelity/assignment-client/entities/models.json.gz"))
  (when is-macintosh-machine
    (find-file "~/Library/Application Support/High Fidelity/Stack Manager/resources/models.json.gz")))

(defun ajt-load-common-files ()
  "Load common files"
  (interactive)
  (let ((code-path ajt-hifi-path)
        (docs-path "~/docs"))
    (find-file (concat code-path "/interface/src/Application.cpp"))
    (find-file (concat code-path "/libraries/avatars/src/AvatarData.cpp"))
    (find-file (concat code-path "/libraries/avatars-renderer/src/avatars-renderer/Avatar.cpp"))
    (find-file (concat code-path "/interface/src/avatar/MyAvatar.cpp"))
    (find-file (concat code-path "/libraries/render-utils/src/Model.cpp"))
    (find-file (concat code-path "/libraries/avatars-renderer/src/avatars-renderer/SkeletonModel.cpp"))
    (find-file (concat code-path "/libraries/render-utils/src/CauterizedModel.cpp"))
    (find-file (concat code-path "/interface/src/avatar/MySkeletonModel.cpp"))
    (find-file (concat code-path "/interface/resources/avatar/avatar-animation.json"))
    (find-file (concat code-path "/libraries/animation/src/AnimSkeleton.cpp"))
    (find-file (concat code-path "/libraries/animation/src/AnimInverseKinematics.cpp"))
    (find-file (concat code-path "/libraries/animation/src/Rig.cpp"))
    (find-file (concat code-path "/libraries/shared/src/glmHelpers.cpp"))
    (find-file (concat code-path "/libraries/fbx/src/FBXReader.cpp"))

    (find-file (concat docs-path "/hifi/todo.md"))
    (find-file (concat code-path "/scripts/system/controllers/controllerDispatcher.js"))))

;; eslint
(defun ajt-eslint ()
  (interactive)
  (shell-command (concat "eslint -f unix " (buffer-file-name)) "*eslint-log*")
  (pop-to-buffer "*eslint-log*")
  (compilation-mode))

;; jsonlint
(defun ajt-jsonlint ()
  (interactive)
  (shell-command (concat "jsonlint \"" (buffer-file-name) "\"") "*jsonlint-log*")
  (pop-to-buffer "*jsonlint-log*")
  (compilation-mode))

;; execute a function for each line in the current buffer
(defun ajt-for-each-line (f)
  (goto-char 1)
  (let ((more-lines 't)
        (start-point 0))
    (while more-lines
      (beginning-of-line)
      (setq start-point (point))
      (end-of-line)
      (funcall f (buffer-substring-no-properties start-point (point)))
      (setq more-lines (= 0 (forward-line 1))))))

(require 'subr-x)
(defun ajt-stuck-resources-count ()
  (interactive)
  (setq count 0)
  (let ((table (make-hash-table :test 'equal)))
    (ajt-for-each-line
     (lambda (line)
       (let* ((starting-str "[hifi.networking.resource] Starting request for: ")
              (finished-str "[hifi.networking] Finished loading: ")
              (starting-index (cl-search starting-str line))
              (finished-index (cl-search finished-str line)))
         ;;(print (list "starting-index -> " starting-index ", finished-index -> " finished-index))
         (when starting-index
           (let* ((key (substring line (+ starting-index (length starting-str)) (length line)))
                  (count (gethash key table 1)))
             (print (format "starting, key = %s, count = %d" key count))
             (puthash key count table)))
         (when finished-index
           (let* ((key (substring line (+ finished-index (length finished-str)) (length line)))
                  (count (gethash key table 0)))
             (print (format "finished, key = %s, count = %d" key count))
             (if (eq count 1)
                 (remhash key table)
               (puthash key (- count 1) table)))))))
    (let ((keys (hash-table-keys table)))
      (print (format "Found %d open resources" (length keys)))
      (mapcar (lambda (x) (print (format "    %s, count = %d" x (gethash x table)))) keys))))



