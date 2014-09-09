;; alex-paredit.el --- paredit

(package-require 'paredit)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook    'enable-paredit-mode)

(provide 'alex-paredit)
