; Prerequisite configuration:
; $ rustup component add rust-src
; $ cargo install racer

(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (use-package flycheck-rust)
  (use-package racer
    :diminish racer-mode
    :config
    (use-package company-racer)
    (setq racer-rust-src-path
      "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
    (add-hook 'racer-mode-hook (lambda ()
      (eldoc-mode)
      (company-mode)
      (diminish 'eldoc-mode))))
  :config
  (setq rust-rustfmt-bin "~/.cargo/bin/rustfmt")
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook (lambda ()
    (racer-mode)
    (setq fill-column 100)))
  :bind (:map rust-mode-map
         ([tab] . company-indent-or-complete-common)))

(provide 'my-rust-config)
