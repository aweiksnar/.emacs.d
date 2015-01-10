;;; init.el --- alex's init file

(require 'org-install)
(require 'ob-tangle)
;; (setq debug-on-error t)
(org-babel-load-file
 (expand-file-name "alex.org"
                   user-emacs-directory))
