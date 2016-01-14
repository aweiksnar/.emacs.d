;;; init.el --- alex's init file

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; (setq debug-on-error t)

(org-babel-load-file
 (expand-file-name "alex.org"
                   user-emacs-directory))
