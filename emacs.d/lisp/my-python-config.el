; TODO: Automatically enable/disable virtualenvs when entering/leaving files

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :config
  (use-package elpy
    :init
    (elpy-enable)
    :config
    (setq elpy-rpc-python-command "python3")
    (setq elpy-formatter "black")
    (add-hook 'elpy-mode-hook (lambda ()
      (add-hook 'before-save-hook 'elpy-format-code nil t))))
  (setq flycheck-python-pylint-executable "pylint3")
  (setq flycheck-python-pycompile-executable "python3"))

(provide 'my-python-config)
