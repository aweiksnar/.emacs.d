;;; alex-eco.el --- embedded coffeescript templates settings

(setq auto-mode-alist
      (append '((".*\\.eco\\'" . html-mode))
              auto-mode-alist))

(provide 'alex-eco)
