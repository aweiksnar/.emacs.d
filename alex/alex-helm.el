;;; alex-coffee-mode --- hot coffee

(package-require 'helm)

(global-set-key (kbd "C-c h") 'helm-find-files)

(helm-mode 1)

(provide 'alex-helm)