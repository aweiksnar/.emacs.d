;;; alex-fiplr.el --- find-file-in-project settings

;; Find files: M-x fiplr-find-file RET
;; Find directories: M-x fiplr-find-directory RET
;; Clear caches: M-x fiplr-clear-cache RET

(setq fiplr-root-markers '(".git" ".svn"))

(setq fiplr-ignored-globs '((directories (".git" ".svn" "node_modules"))
                            (files ("*.jpg" "*.png" "*.zip" "*~"))))

(global-set-key (kbd "C-x f") 'fiplr-find-file)

(provide 'alex-fiplr)
