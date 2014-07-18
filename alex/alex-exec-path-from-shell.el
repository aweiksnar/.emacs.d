;;; alex-exec-path-from-shell.el --- exec-path-from-shell
(package-require 'exec-path-from-shell)

;; get PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'alex-exec-path-from-shell)
