;;; alex-styl.el --- stylus settings

;; highlight stylus files as css
(setq auto-mode-alist
      (append '((".*\\.styl\\'" . css-mode))
              auto-mode-alist))

(provide 'alex-styl)
