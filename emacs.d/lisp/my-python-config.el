; Prerequisite configuration:
; $ sudo apt-get install python3-jedi pylint3 virtualenv

; TODO: Automatically enable/disable virtualenvs when entering/leaving files

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :config
  (use-package company-jedi
    :config
    (add-to-list 'company-backends 'company-jedi))
  (use-package virtualenvwrapper
    :config
    (setq venv-location "~/.venv/wrk/"))
  (use-package py-autopep8
    :config
    (setq py-autopep8-options '("--aggressive" "--aggressive"))
    (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))
  (add-hook 'python-mode-hook #'company-mode)
  (setq flycheck-python-pylint-executable "pylint3")
  (setq flycheck-python-pycompile-executable "python3")
  :bind (:map python-mode-map
          ([tab] . company-indent-or-complete-common)))

(provide 'my-python-config)
