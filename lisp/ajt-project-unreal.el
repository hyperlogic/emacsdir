(load "ajt-vector-math")


(setq ajt-unreal-project-path "C:/msys64/home/ajthy/code/vrdj-school")
(setq ajt-unreal-engine-path "C:/msys64/home/ajthy/code/UnrealEngine/Engine/Source/Runtime")

;; unreal project cpp search with regex
(defun ajt-unreal-project-cpp-search (arg)
  "Search for a regex in all unreal project cpp files"
  (interactive "sunreal-project-cpp:")
  (ajt-grep-find arg (list (concat ajt-unreal-project-path "/Source")) '("*.h" "*.cpp")))

;; unreal plugins cpp search with regex
(defun ajt-unreal-plugins-cpp-search (arg)
  "Search for a regex in all unreal plugin cpp files"
  (interactive "sunreal-plugins-cpp:")
  (ajt-grep-find arg (list (concat ajt-unreal-project-path "/Plugins")) '("*.h" "*.cpp")))

;; unreal engine cpp search with regex
(defun ajt-unreal-engine-cpp-search (arg)
  "Search for a regex in all unreal engine cpp files"
  (interactive "sunreal-engine-cpp:")
  (ajt-grep-find arg (list (concat ajt-unreal-engine-path "")) '("*.h" "*.cpp")))

(global-set-key [f9] 'ajt-unreal-project-cpp-search)
(global-set-key [f10] 'ajt-unreal-plugins-cpp-search)
(global-set-key [f11] 'ajt-unreal-engine-cpp-search)

