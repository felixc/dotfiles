(use-package lsp-mode
  :config
    (use-package lsp-ui
      :commands lsp-ui-mode))

(use-package typescript-mode
  :hook (typescript-mode . lsp-deferred))

(provide 'my-typescript-config)
