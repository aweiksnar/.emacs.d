;;; alex-cjsx.el -- cjsx

;; cjsx files as coffee
(setq auto-mode-alist
      (append '((".*\\.cjsx\\'" . coffee-mode))
              auto-mode-alist))

(provide 'alex-cjsx)
