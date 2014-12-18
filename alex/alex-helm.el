;;; alex-coffee-mode --- hot coffee

(package-require 'helm)

(global-set-key (kbd "C-c h") 'helm-find-files)

;; M-y to helm-show-kill-ring
;; http://sachachua.com/blog/2014/12/emacs-m-y-helm-show-kill-ring/
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(helm-mode 1)

(provide 'alex-helm)
