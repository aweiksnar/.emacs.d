;;; init.el

;; use deeper-blue by default - don't touch this...
(custom-set-variables
 '(custom-enabled-themes (quote (deeper-blue))))
(custom-set-faces)

;; disable menu-bar-mode, tool-bar-mode, scroll-bar-mode
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

;; Add .emacs.d/alex to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "alex"))

;; marmalade
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defvar my-packages '(better-defaults
                      coffee-mode
                      magit
                      rainbow-mode
                      web-mode
                      clojure-mode
                      clojure-test-mode
                      cider
                      fiplr
                      org
                      exec-path-from-shell))

(dolist (p my-packages)
  (package-require p))

;;*replaced by package-require call above
;;(dolist (p my-packages)
;;  (when (not (package-installed-p p))
;;    (package-install p)))

;; require alex-* config files
(setq alex-pkg-full
      '(alex-emacs-prefs
        alex-keys
        alex-dired
        alex-org
        alex-magit
        alex-fiplr
        alex-rainbow-mode
        alex-eco
        alex-styl
        alex-exec-path-from-shell
        ))

(dolist (file alex-pkg-full)
  (require file))
