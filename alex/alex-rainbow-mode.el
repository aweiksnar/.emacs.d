;; alex-rainbow-mode.el --- rainbow-mode settings
(package-require 'rainbow-mode)

(defun activate-rainbow-mode ()
  (rainbow-mode 1))

(defun stylus-rainbow-mode ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.styl\\'" buffer-file-name))
    (activate-rainbow-mode)))

;; use rainbow-mode in css
(add-hook 'css-mode-hook 'activate-rainbow-mode)

;; use rainbow-mode in stylus
(add-hook 'find-file-hook 'stylus-rainbow-mode)

(provide 'alex-rainbow-mode)
