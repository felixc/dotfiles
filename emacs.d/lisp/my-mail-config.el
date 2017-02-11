(add-to-list 'auto-mode-alist '("/tmp/mutt.*" . mail-mode))

(add-hook 'mail-mode-hook (lambda ()
  (setq-local fill-column 76)

  (defface mail-double-quoted-text-face
    '((((class color)) :foreground "SteelBlue"))
    "Double-quoted email." :group 'mail-faces)
  (defface mail-treble-quoted-text-face
    '((((class color)) :foreground "SlateGrey"))
    "Treble-quoted email." :group 'mail-faces)
  (defface mail-multiply-quoted-text-face
    '((((class color)) :foreground "DarkSlateGrey"))
    "Multiply-quoted email." :group 'mail-faces)

  (font-lock-add-keywords 'mail-mode
   '(("^\\(\\( *>\\)\\{4,\\}\\)\\(.*\\)$"
      (1 'font-lock-comment-delimiter-face)
      (3 'mail-multiply-quoted-text-face))
     ("^\\(\\( *>\\)\\{3\\}\\)\\(.*\\)$"
      (1 'font-lock-comment-delimiter-face)
      (3 'mail-treble-quoted-text-face))
     ("^\\( *> *>\\)\\(.*\\)$"
      (1 'font-lock-comment-delimiter-face)
      (2 'mail-double-quoted-text-face))))))

(provide 'my-mail-config)
