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
  (use-package rainbow-mode :diminish rainbow-mode)
  (rainbow-mode)
  (setq css-indent-offset 2)))

(use-package rjsx-mode
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook '(lambda ()
    (setq fill-column 100)
    (setq js-indent-level 2)))
  (add-hook 'js2-mode-hook 'prettier-js-mode))

(provide 'my-webdev-config)
