;;; magit.el --  git integration

(package-require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'alex-magit)
