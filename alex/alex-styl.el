;;; alex-styl.el --- stylus settings

(setq auto-mode-alist
      (append '((".*\\.styl\\'" . css-mode))
              auto-mode-alist))

(provide 'alex-styl)
