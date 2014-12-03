;;; alex-coffee-mode --- hot coffee

(package-require 'coffee-mode)

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(setq coffee-js-mode 'js2-mode)

;; patch coffee-mode so coffee-compile-region pops up a new
;; non-focused window instead of replacing the current buffer.
;; from https://gitlab.com/bodil/emacs-d/blob/master/bodil/bodil-js.el
(eval-after-load "coffee-mode"
  '(defun coffee-compile-region (start end)
     "Compiles a region and displays the JS in another buffer."
     (interactive "r")
     (let ((buffer (get-buffer coffee-compiled-buffer-name)))
       (when buffer (kill-buffer buffer)))
     (call-process-region start end coffee-command nil
                          (get-buffer-create coffee-compiled-buffer-name) nil "-s" "-p" "--bare")
     (let ((buffer (get-buffer coffee-compiled-buffer-name)))
       (with-current-buffer buffer
         (funcall coffee-js-mode)
         (goto-char (point-min)))
       (display-buffer buffer))))

(provide 'alex-coffee-mode)



