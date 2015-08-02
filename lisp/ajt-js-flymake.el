(require 'flymake)

;(print (search-forward-regexp "\\([^:]+\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$"))
;(string-match "^\\([^:]+\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$" "Main_flymake.js: line 147, col 9, Mixed spaces and tabs.")
;(match-string 3 "Main_flymake.js: line 147, col 9, Mixed spaces and tabs.")

; command for google-closure-compiler
; java -jar google-closure-compiler.jar --js foo.js

; command for jslint
; assuming sudo npm install -g jslint
; jslint foo.js

; command for jshint
; assuming sudo npm install -g jshint
; jshint foo.js
(defun flymake-jshint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
           (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
      (list "jshint" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
			 '(".+\\.js$" flymake-jshint-init))

(add-to-list 'flymake-err-line-patterns
			 '("\\([^:]+\\): line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.+\\)$" 1 2 3 4))

(provide 'ajt-js-flymake)




