;;; alex-fiplr.el --- find-file-in-project settings

;; Find files: M-x fiplr-find-file RET
;; Find directories: M-x fiplr-find-directory RET
;; Clear caches: M-x fiplr-clear-cache RET

(package-require 'fiplr)

(setq fiplr-root-markers '(".git" ".svn"))

;; ignore these in file finder
(setq fiplr-ignored-globs '((directories (".git" ".svn" "node_modules"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))

;; fuzzy file finder in project
(global-set-key (kbd "C-x f") 'fiplr-find-file)

(provide 'alex-fiplr)
