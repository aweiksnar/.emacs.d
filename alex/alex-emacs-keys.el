;;; alex-keys.el --- general key bindings

;; toggle whitespace
(global-set-key (kbd "C-x w") 'whitespace-mode)

;; hippie expand completion
(global-set-key (kbd "M-/") 'hippie-expand)

;; open eshell
(global-set-key (kbd "C-x t") 'eshell)

;; rgrep
(global-set-key (kbd "C-x e") 'rgrep)

(global-set-key (kbd "C-c o")
                (lambda ()
                  (interactive)
                  (find-file "~/org/")))

;; shift + arrow key to move between windows
(windmove-default-keybindings)

(put 'downcase-word 'disabled t)

(provide 'alex-emacs-keys)
