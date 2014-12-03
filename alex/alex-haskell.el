;;; alex-haskell.el --- haskell

(package-require 'haskell-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(provide 'alex-haskell)
