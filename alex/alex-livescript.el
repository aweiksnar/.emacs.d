;;; alex-livescript-mode --- LiveScript

(package-require 'coffee-mode)

;; ls as coffee
(setq auto-mode-alist
      (append '((".*\\.ls\\'" . coffee-mode))
              auto-mode-alist))

(provide 'alex-livescript)
