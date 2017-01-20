; Prerequisite configuration:
; $ rustup component add rust-src
; $ cargo install racer

(require 'rust-mode)

(setq racer-rust-src-path
  "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")

(add-hook 'rust-mode-hook (lambda ()
  (setq fill-column 100)
  (racer-mode)))

(add-hook 'racer-mode-hook (lambda ()
  (eldoc-mode)
  (company-mode)))

(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

(provide 'my-rust-config)
