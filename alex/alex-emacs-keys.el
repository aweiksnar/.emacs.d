;;; alex-keys.el --- general key bindings

;; toggle whitespace
(global-set-key (kbd "C-x w") 'whitespace-mode)

;; hippie expand completion
(global-set-key (kbd "M-/") 'hippie-expand)

;; open ansi-term
(global-set-key (kbd "C-x t") 'ansi-term)

;; rgrep
(global-set-key (kbd "C-x e") 'rgrep)

;; shift + arrow key to move between windows
(windmove-default-keybindings)

(put 'downcase-word 'disabled t)

(provide 'alex-emacs-keys)
