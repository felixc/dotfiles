; Prerequisite configuration:
; $ sudo apt-get install python3-jedi pylint3 virtualenv
; M-x jedi:install-server

(setq flycheck-python-pylint-executable "pylint3")

(add-hook 'python-mode-hook (lambda ()
  (add-to-list 'company-backends 'company-jedi)
  (company-mode)
  (define-key python-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (define-key company-active-map [tab] 'company-complete-common-or-cycle)))

(provide 'my-python-config)
