(use-package lsp-mode
  :diminish 'lsp-mode
  :config
  (use-package lsp-ui
    :custom
      (lsp-ui-doc-enable nil)
    :bind (:map lsp-ui-mode-map
            ([tab] . company-indent-or-complete-common)
            ("C-?" . lsp-ui-doc-glance)
            ("M-." . lsp-ui-peek-find-definitions)
            ("M-?" . lsp-ui-peek-find-references))))

(provide 'my-language-server-config)
