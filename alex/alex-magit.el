;;; magit.el --  git integration

;; TODO: should I require packages in config files here, or keep in init.el?
;; (package-require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'alex-magit)
