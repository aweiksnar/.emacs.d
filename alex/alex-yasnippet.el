;;; alex-yasnippet.el --- yasnippets

;; (package-require 'popup)
(package-require 'yasnippet)
(package-require 'yasnippet-bundle)
(yas/initialize)
(setq yas/root-directory (concat dotfiles-dir "snippets"))
(yas/load-directory
 (concat dotfiles-dir "snippets"))
(yas/global-mode 1)

(provide 'alex-yasnippet)
