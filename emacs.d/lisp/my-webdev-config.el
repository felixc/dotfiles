; Better JS and CSS editing in HTML documents
(use-package multi-web-mode
  :custom
    (mweb-default-major-mode 'html-mode)
    (mweb-tags '((js-mode "<script[^>]*>" "</script>")
                 (css-mode "<style[^>]*>" "</style>")))
    (mweb-filename-extensions '("html"))
  :config
    (multi-web-global-mode t))

; Show colours as represented by strings
(add-hook 'css-mode-hook (lambda ()
  (use-package rainbow-mode :diminish rainbow-mode)
  (rainbow-mode)
  (setq css-indent-offset 2)))

(use-package rjsx-mode
  :mode "\\.js\\'"
  :hook
    (js2-mode . (lambda ()
      (setq fill-column 100)
      (setq js-indent-level 2)))
    (js2-mode . prettier-js-mode))

(provide 'my-webdev-config)
