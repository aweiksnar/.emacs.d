;;; alex-coffee-mode --- hot coffee

(package-require 'coffee-mode)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(provide 'alex-coffee-mode)
