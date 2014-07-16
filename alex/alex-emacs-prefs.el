;;; alex-emacs-prefs.el --- general emacs prefs

;; prevent pop-ups on osx
(setq ns-pop-up-frames nil)

;; use tabs instead of spaces
(setq-default indent-tabs-mode nil)

;; set tab-width to 2
(setq-default tab-width 2)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; get rid of left and right side-space
(set-fringe-mode 0)

;; auto-close parens
(electric-pair-mode 1)

(provide 'alex-emacs-prefs)
