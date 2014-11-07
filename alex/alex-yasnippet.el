;;; alex-yasnippet.el --- yasnippets

;; (package-require 'popup)
(package-require 'yasnippet)
(package-require 'yasnippet-bundle)
(yas-global-mode 1)
;; (yas-initialize)

;; prevent yasnippet from indenting
(setq yas-indent-line nil)

(setq yas-root-directory
      (concat dotfiles-dir "snippets"))

(yas-load-directory
 (concat dotfiles-dir "snippets"))

(provide 'alex-yasnippet)
