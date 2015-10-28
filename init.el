;;; init.el --- alex's init file

;; (package-initialize) in alex.org, commented here for emacs

(require 'org-install)
(require 'ob-tangle)
(setq debug-on-error t)
(org-babel-load-file
 (expand-file-name "alex.org"
                   user-emacs-directory))
