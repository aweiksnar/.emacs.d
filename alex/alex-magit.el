;;; magit.el --  git integration

(package-require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

;; fix a weird magit issue with the wombat color scheme
;; highlighting the selected hunk in diffs
(defun disable-magit-highlight-in-buffer ()
  (face-remap-add-relative 'magit-item-highlight '()))

(add-hook 'magit-status-mode-hook 'disable-magit-highlight-in-buffer)

(provide 'alex-magit)
