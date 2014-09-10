;; alex-paredit.el --- paredit

(package-require 'paredit)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook    #'enable-paredit-mode)

(provide 'alex-paredit)
