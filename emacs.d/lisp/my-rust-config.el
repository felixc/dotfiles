; Prerequisite: rustup component add rls rust-analysis rust-src rustfmt

(use-package rust-mode
  :hook (rust-mode . lsp-deferred)
  :config
    (add-to-list 'exec-path "~/.cargo/bin")
    (setq rust-format-on-save t)
    (setq rust-rustfmt-bin "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin/rustfmt")
    (add-hook 'rust-mode-hook (lambda ()
      (setq fill-column 100)))
    (use-package flycheck-rust
      :config
        (with-eval-after-load 'rust-mode
          (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

(provide 'my-rust-config)
