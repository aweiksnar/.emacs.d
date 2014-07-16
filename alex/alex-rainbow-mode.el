;; alex-rainbow-mode.el --- rainbow-mode settings

(defun activate-rainbow-mode ()
  (rainbow-mode 1))

;; use rainbow-mode in stylus
(defun stylus-rainbow-mode ()
  (when (and (stringp buffer-file-name)
             (string-match "\\.styl\\'" buffer-file-name))
    (activate-rainbow-mode)))

;; use rainbow-mode in css
(add-hook 'css-mode-hook 'activate-rainbow-mode)

(add-hook 'find-file-hook 'stylus-rainbow-mode)

(provide 'alex-rainbow-mode)
