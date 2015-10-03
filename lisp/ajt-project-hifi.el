(setq ajt-hifi-path "~/code/hifi")
(when (and is-windows-machine (not (string= hostname "blackholesun")))
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


(defun ajt-load-common-files ()
  "Load common files"
  (interactive)
  (let ((code-path "~/code/hifi")
        (docs-path "~/docs"))
    (find-file (concat code-path "/interface/src/Application.cpp"))
    (find-file (concat code-path "/interface/src/avatar/MyAvatar.cpp"))
    (find-file (concat code-path "/interface/resources/meshes/defaultAvatar_full/avatar-animation.json"))
    (find-file (concat code-path "/libraries/animation/src/AnimSkeleton.cpp"))
    (find-file (concat code-path "/libraries/animation/src/Rig.cpp"))
    (find-file (concat docs-path "/hifi/todo.md"))))
