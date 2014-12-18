;;; init.el

;; use deeper-blue by default - don't touch this...
(custom-set-variables
 '(custom-enabled-themes (quote (deeper-blue))))
(custom-set-faces)

;; disable menu-bar-mode, tool-bar-mode, scroll-bar-mode
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; winner mode
(when (fboundp 'winner-mode)
     (winner-mode 1))

;; abbrev mode
;; (setq-default abbrev-mode t)

;; emacs 24.3 pasteboard error fix
(setq save-interprogram-paste-before-kill nil)

;; fix ls --dired startup error
(setq dired-use-ls-dired nil)

;; (electric-indent-mode 0) ; make return key not auto indent

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

;; set backup directory
(setq backup-directory-alist `(("." . "~/.saves")))

;; Add .emacs.d/alex to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path (concat dotfiles-dir "alex"))
(add-to-list 'load-path (concat dotfiles-dir "alex/vendor"))

;; marmalade / melpa
(require 'package)
(dolist (source '(("melpa" . "http://melpa.org/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives source t))

(package-initialize)

;; require alex-* files
(setq alex-pkg-full
      '(alex-emacs-eshell
        alex-emacs-prefs
        alex-emacs-keys
        alex-exec-path-from-shell
        alex-ace-jump-mode
        alex-better-defaults
        alex-clojure
        alex-coffee-mode
        alex-dired
        alex-eco
        alex-elisp
        alex-exec-path-from-shell
        alex-fiplr
        alex-haskell
        alex-helm
        alex-js
        alex-magit
        alex-org
        alex-livescript
        alex-paredit
        alex-rainbow-mode
        alex-styl
        alex-sml-mode
        alex-web-mode
        alex-cjsx
        alex-yasnippet
        ))

(dolist (file alex-pkg-full)
  (require file))
