;;; alex-exec-path-from-shell.el --- settings for exec-path-from-shell

;; get PATH
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; fix eshell node repl weirdness
(setenv "NODE_NO_READLINE" "1")

(provide 'alex-exec-path-from-shell)
