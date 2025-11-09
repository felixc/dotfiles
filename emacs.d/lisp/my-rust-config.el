; Prerequisite: rustup component add rls rust-analysis rust-src rustfmt

(use-package rust-mode
  :hook
    (rust-mode . lsp-deferred)
    (rust-mode . (lambda () (setq fill-column 100)))
  :custom
    (rust-format-on-save t)
    (rust-rustfmt-bin "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rustfmt")
  :config
    (add-to-list 'exec-path "~/.cargo/bin")
    (use-package flycheck-rust
      :config
        (with-eval-after-load 'rust-mode
          (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

(provide 'my-rust-config)
