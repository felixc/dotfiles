; TODO: Automatically enable/disable virtualenvs when entering/leaving files

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :custom
    (flycheck-python-pylint-executable "pylint3")
    (flycheck-python-pycompile-executable "python3")
  :config
    (use-package elpy
      :custom
        (elpy-rpc-python-command "python3")
        (elpy-formatter "black")
      :hook
        (elpy-mode . (lambda ()
          (add-hook 'before-save-hook 'elpy-format-code nil t)))
      :config
        (elpy-enable)))

(provide 'my-python-config)
