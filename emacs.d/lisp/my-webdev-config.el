; Better JS and CSS editing in HTML documents
(use-package multi-web-mode
  :config
  (setq mweb-default-major-mode 'html-mode)
  (setq mweb-tags '((js-mode "<script[^>]*>" "</script>")
                    (css-mode "<style[^>]*>" "</style>")))
  (setq mweb-filename-extensions '("html"))
  (multi-web-global-mode t))

; Show colours as represented by strings
(add-hook 'css-mode-hook '(lambda ()
  (rainbow-mode)
  (setq css-indent-offset 2)))

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

  (defun prefix-js2-mode-match-curly ()
    "Call js2-mode's match-curly with a prefix argument to avoid a newline."
    (interactive)
    (let ((current-prefix-arg '(1)))
      (call-interactively 'js2-mode-match-curly)))

  (add-hook 'js2-mode-hook '(lambda ()
    (setq fill-column 100)
    (setq js2-basic-indent 2)
    (define-key js2-mode-map (read-kbd-macro "{") 'prefix-js2-mode-match-curly))))

(provide 'my-webdev-config)
