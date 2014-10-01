;;; alex-clojure.el --- clojure settings

(package-require 'clojure-mode)
(package-require 'clojure-test-mode)
(package-require 'cider)
(package-require 'om-mode)

(add-to-list 'auto-mode-alist '("\\.cljs" . clojure-mode))

(provide 'alex-clojure)
