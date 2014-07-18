;;; alex-eco.el --- embedded coffeescript templates settings

;; highlight .eco files as html
(setq auto-mode-alist
      (append '((".*\\.eco\\'" . html-mode))
              auto-mode-alist))

(provide 'alex-eco)
