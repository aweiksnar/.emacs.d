;; alex-paredit.el --- paredit

(package-require 'paredit)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(setq paredit-hooks
      '(emacs-lisp-mode-hook
        clojure-mode-hook))

(dolist (hook paredit-hooks)
  (add-hook hook #'enable-paredit-mode))

(provide 'alex-paredit)
